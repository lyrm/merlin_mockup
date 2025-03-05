(** Some config*)
let len = 100

(** config is the index at which the computation stop *)
(* let config = 12 *)

let debug = true

(** Mutable state *)
let shared_value = Array.make len 0

(** Mpipeline *)
module Mpipeline = struct
  type t = int * int array

  type shared = {
    message : [ `Waiting | `Nothing | `Cancel ] Atomic.t;
    closed : [ `True | `False | `Closed | `Exn of exn ] Atomic.t;
    curr_config : int option Shared.t;
    partial_result : t option Shared.t;
    result : t option Shared.t;
  }

  (** [create_shared] *)
  let create_shared () =
    {
      message = Atomic.make `Nothing;
      closed = Atomic.make `False;
      curr_config = Shared.create None;
      partial_result = Shared.create None;
      result = Shared.create None;
    }

  (** [share_exn]: called by the typer domain *)
  let rec share_exn (shared : shared) exn =
    let string_domain = Utils.string_domain () in
    if debug then Format.printf "%sEnter Share_exn.\n%!" string_domain;
    match Atomic.get shared.closed with
    | `False ->
        let exn_v = `Exn exn in
        if Atomic.compare_and_set shared.closed `False exn_v then (
          if debug then
            Format.printf "%sShare_exn : succeed CAS.\n%!" string_domain;
          while Atomic.get shared.closed == exn_v do
            Shared.signal shared.partial_result
          done)
        else (
          if debug then
            Format.printf "%sShare_exn : failed CAS.\n%!" string_domain;
          share_exn shared exn)
    | `True -> ()
    | _ -> assert false

  (** [closing]: called by the main domain *)
  let rec closing shared =
    let string_domain = Utils.string_domain () in
    if debug then Format.printf "%sEnter Closing.\n%!" string_domain;

    match Atomic.get shared.closed with
    | `False ->
        if Atomic.compare_and_set shared.closed `False `True then (
          if debug then
            Format.printf "%sClosing : succeed CAS.\n%!" string_domain;
          while Atomic.get shared.closed == `True do
            Shared.signal shared.curr_config
          done)
        else (
          if debug then
            Format.printf "%sClosing : failed CAS.\n%!" string_domain;
          closing shared)
    | `Exn exn ->
        Atomic.set shared.closed `True;
        raise exn
    | `True -> failwith "Closing: `True"
    | `Closed -> failwith "Closing: `Closed"

  (** [cancel] is called by the main domain *)
  let rec cancel (shared : shared) =
    let string_domain = Utils.string_domain () in
    if debug then Format.printf "%sEnter CANCEL\n%!" string_domain;

    match Atomic.get shared.message with
    | `Nothing as prev ->
        if Atomic.compare_and_set shared.message prev `Cancel then
          while Atomic.get shared.message = `Cancel do
            Shared.signal shared.curr_config
          done
        else cancel shared
    | _ -> failwith "Cancel."

  exception Bug of int

  (** [typer] *)
  let typer config shared =
    let exception Break in
    let string_domain = Utils.string_domain () in
    let first = ref true in
    try
      for ind = 0 to len - 1 do
        if debug && ind mod 10 = 0 then
          Format.printf "%sTyping step %d in %d\n%!" string_domain ind len;

        (* Active waiting seems ok here as the whole purpose of the typer is to 
        type as fast as possible. Thus its waiting time should be as short as possible. *)
        (match Atomic.get shared.message with
        | `Waiting ->
            if debug then
              Format.printf "%sGive lock to main for analysis.\n%!"
                string_domain;
            while Atomic.get shared.message = `Waiting do
              Domain.cpu_relax ()
            done
        | `Cancel ->
            if debug then
              Format.printf "%sRequest is cancelled.\n%!" string_domain;
            Atomic.set shared.message `Nothing;
            raise Break
        | _ -> ());
        (*  Where typing happens *)
        try
          Shared.protect shared.result (fun () ->
              let res = ind in
              if Random.int 10 == 0 then raise (Bug config);
              shared_value.(ind) <- res + shared_value.(ind);
              if ind >= config && !first then (
                (* [first] enables to put >= instead of = which is going to be 
              useful with the cash (typing can begin after the line we want to 
              stop if the beginning is already in cash)*)
                first := false;
                Shared.locking_set shared.partial_result
                  (Some (config, shared_value)));
              (* Simulating some additional work*)
              Utils.stupid_work () |> ignore)
        with exn ->
          if ind >= config then (* TODO : log the exception *) () else raise exn
      done
    with Break -> ()

  (** [make] *)
  let make config shared =
    let string_domain = Utils.string_domain () in
    if debug then Format.printf "%sBegin config %d\n%!" string_domain config;
    typer config shared;
    if debug then
      Format.printf "%sFinish typing config %d\n%!" string_domain config

  (** [domain_typer] *)
  let domain_typer shared () =
    let string_domain = Utils.string_domain () in

    let rec loop () =
      if debug then Format.printf "%sLooping\n%!" string_domain;

      if Atomic.get shared.closed = `True then (
        if debug then Format.printf "%sClosed\n%!" string_domain;
        Atomic.set shared.closed `Closed)
      else
        match Shared.get shared.curr_config with
        | None ->
            if debug then
              Format.printf "%sWaiting for a config\n%!" string_domain;
            Shared.wait shared.curr_config;
            loop ()
        | Some config ->
            Shared.set shared.curr_config None;
            (try
               make config shared;
               Shared.locking_set shared.result (Some (config, shared_value))
             with exn -> share_exn shared exn);
            loop ()
    in
    if debug then Format.printf "%sBegin\n%!" string_domain;
    Shared.protect shared.curr_config loop

  (** [check_message] *)
  (* let check_message_main shared config continue =
    let string_domain = Utils.string_domain () in

    match Atomic.get shared.closed with
    | `True | `Closed -> assert false
    | `Exn (Bug buggy_config) ->
        if debug then
          Format.printf "%sWitness the exception of config %d.\n%!"
            string_domain buggy_config;
        Atomic.set shared.closed `False;
        if buggy_config == config then None else failwith "Should not happen"
    | _ -> continue () *)

  (** [get] *)
  let get shared config =
    let string_domain = Utils.string_domain () in
    if debug then
      Format.printf "%sWait for lock to change config.\n%!" string_domain;

    Shared.locking_set shared.curr_config (Some config);

    if debug then Format.printf "%sConfig changed.\n%!" string_domain;

    let rec loop () =
      let critical_section () =
        match Shared.get shared.partial_result with
        | None -> (
            (* TODO : put this in a function and make sure it is called enough (to avoid missing a message) *)
            match Atomic.get shared.closed with
            | `True | `Closed -> assert false
            | `Exn (Bug buggy_config) ->
                if debug then
                  Format.printf "%sWitness the exception of config %d.\n%!"
                    string_domain buggy_config;
                Atomic.set shared.closed `False;
                if buggy_config == config then `Result None else `Retry
            | _ ->
                if debug then
                  Format.printf "%sWaiting for a result\n%!" string_domain;
                Shared.wait shared.partial_result;
                `Retry)
        | Some ((_rconfig, _) as pipeline) ->
            if debug then
              Format.printf "%sSome partial result \n%!" string_domain;

            (* if _rconfig != config then (
              Format.printf
                "%sMismatching config %d / curr : %d (curr_config = %s)\n%!"
                string_domain rconfig config
                (Option.fold ~none:"None" ~some:string_of_int
                   (Shared.get shared.curr_config));
              failwith "problem"); *)
            Shared.set shared.partial_result None;
            `Result (Some pipeline)
      in
      match Shared.protect shared.partial_result critical_section with
      | `Retry -> loop ()
      | `Result r -> r
    in
    loop ()
end

(** [run_analysis]*)
let run_analysis pipeline _ind_partial =
  for i = 0 to len - 1 do
    pipeline.(i) <- 10 * pipeline.(i)
  done

(** [analysis]*)
let analysis (shared : Mpipeline.shared)
    ((config_typer, partial_pipeline) : Mpipeline.t) config =
  let string_domain = Utils.string_domain () in
  if debug then Format.printf "%sBegin analysis.\n%!" string_domain;

  if config_typer <> config then failwith "impossible ?";
  (* Main domain signals it wants the lock  *)
  if Atomic.compare_and_set shared.message `Nothing `Waiting then (
    if debug then Format.printf "%sWaiting to get the lock.\n%!" string_domain;

    Shared.protect shared.result (fun () ->
        Atomic.set shared.message `Nothing;
        run_analysis partial_pipeline config))
  else failwith "Should no happen";
  if debug then Format.printf "%sAnalysis finished.\n%!" string_domain

(** [random_config] *)
let random_config () = Random.int 5

(** [run] = New_merlin.run *)
let run shared count_max =
  let string_domain = Utils.string_domain () in

  let rec do_one_request prev count =
    if count = count_max then ()
    else
      let config = random_config () in
      if debug then
        Format.printf "%sRequest number = %d with config = %d.\n%!"
          string_domain count config;
      if prev = config then Mpipeline.cancel shared;
      (* Reinit shared is required : that an issue : when should we reinit ?
      
        -> the typer should be the one that do it 
        -> would require a message to ensure main witness it (but does it need to ?)
      *)
      let r = Mpipeline.get shared config in
      (* Utils.print_arr_int ~prefix:"Partial result :" (snd r); *)
      (match r with Some r -> analysis shared r config | None -> ());
      do_one_request config (count + 1)
  in
  do_one_request (-1) 0

(** [main] = Ocaml_merlin_server.main *)
let main () =
  Random.self_init ();
  let shared = Mpipeline.create_shared () in
  if debug then Format.printf "%sSpawning typer\n%!" (Utils.string_domain ());

  let domain_typer = Domain.spawn @@ Mpipeline.domain_typer shared in

  let _ = run shared 20 in

  if debug then Format.printf "%sRun finished\n%!" (Utils.string_domain ());

  Mpipeline.closing shared;
  Domain.join domain_typer;
  ()
(* Utils.print_arr_int shared_value *)

let () = main ()

(* Bug issueLSP695 *)

(** {
  Is there an issue depending on when an exception is raised by the typer domain ?
  - before a partial result has been returned : No, the main domain is located 
  before the write on `curr_config` and the wait on `partial_result`.  
  - after a partial result has been returned : the main domain can be anywhere. 
  In particular, it could be already processing a new request / config. However, 
  it only becomes aware of the issue when it looks at the `message`. But then 
  the typer is waiting for ACK to continue. So it should be has soon as possible. 
  -> It is not an issue if the reading of `message` is always able to manage the 
    possible caught exception and if it happens often enough. 

*)
