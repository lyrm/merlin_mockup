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
    if debug then Format.printf "%sEnter CANCELn%!" string_domain;

    match Atomic.get shared.message with
    | `Nothing as prev ->
        if Atomic.compare_and_set shared.message prev `Cancel then
          while Atomic.get shared.message = `Cancel do
            Shared.signal shared.curr_config
          done
        else cancel shared
    | _ -> failwith "Cancel."

  exception Bug

  (** [typer] *)
  let typer config shared =
    let exception Break in
    let string_domain = Utils.string_domain () in
    try
      for ind = 0 to len - 1 do
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
        Shared.protect shared.result (fun () ->
            let res = ind in
            if Random.int 10 == 0 then raise Bug;
            shared_value.(ind) <- res + shared_value.(ind);
            if ind = config then
              Shared.locking_set shared.partial_result
                (Some (config, shared_value));
            (* Simulating some additional work*)
            Utils.stupid_work () |> ignore)
      done
    with Break -> ()

  (** [make] *)
  let make config shared =
    let string_domain = Utils.string_domain () in
    if debug then Format.printf "%sBegin config %d\n%!" string_domain config;
    typer config shared

  (** [domain_typer] *)
  let domain_typer shared () =
    let string_domain = Utils.string_domain () in

    let rec loop () =
      if debug then Format.printf "%sLooping\n%!" string_domain;

      if Atomic.get shared.closed = `True then Atomic.set shared.closed `Closed
      else
        match Shared.get shared.curr_config with
        | None ->
            if debug then
              Format.printf "%sWaiting for a config\n%!" string_domain;
            Shared.wait shared.curr_config;
            loop ()
        | Some config -> (
            Shared.set shared.curr_config None;
            try
              make config shared;
              Shared.locking_set shared.result (Some (config, shared_value));
              loop ()
            with exn -> share_exn shared exn)
    in
    if debug then Format.printf "%sBegin\n%!" string_domain;
    Shared.protect shared.curr_config @@ fun () -> loop ()

  (** [get] *)
  let get shared config =
    let string_domain = Utils.string_domain () in

    Shared.locking_set shared.curr_config (Some config);

    let rec loop () =
      match Shared.get shared.partial_result with
      | None -> (
          match Atomic.get shared.closed with
          | `True | `Closed -> assert false
          | `Exn exn ->
              Atomic.set shared.closed `False;
              raise exn
          | _ ->
              if debug then
                Format.printf "%sWaiting for a result\n%!" string_domain;

              Shared.wait shared.partial_result;
              loop ())
      | Some pipeline ->
          if debug then Format.printf "%sSome result \n%!" string_domain;
          Shared.set shared.partial_result None;
          pipeline
    in
    Shared.protect shared.partial_result @@ fun () -> loop ()
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
  if Atomic.compare_and_set shared.message `Nothing `Waiting then
    Shared.protect shared.result (fun () ->
        Atomic.set shared.message `Nothing;
        run_analysis partial_pipeline config)
  else failwith "Should no happen"

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
      analysis shared r config;
      do_one_request config (count + 1)
  in
  do_one_request (-1) 0

(** [main] = Ocaml_merlin_server.main *)
let main () =
  Random.self_init ();
  let shared = Mpipeline.create_shared () in
  if debug then Format.printf "%sSpawning typer\n%!" (Utils.string_domain ());

  let domain_typer = Domain.spawn @@ Mpipeline.domain_typer shared in

  let _ = run shared 10 in

  if debug then Format.printf "%sRun finished\n%!" (Utils.string_domain ());

  Mpipeline.closing shared;
  Domain.join domain_typer;
  ()
(* Utils.print_arr_int shared_value *)

let () = main ()

(* Bug issueLSP695 *)

(** {[
      let domain_typer shared () =
        let rec loop () =
          if Atomic.get shared.closed = `True then
            (* Atomic.set shared.closed `Closed *)
            ()
          else
            match Shared.get shared.curr_config with
            | None ->
                Shared.wait shared.curr_config;
                loop ()
            | Some config -> (
                Shared.set shared.curr_config None;
                try
                  make config shared;
                  Shared.locking_set shared.result (Some (config, shared_value));
                  loop ()
                with exn -> share_exn shared exn)
        in
        Shared.protect shared.curr_config @@ fun () -> loop ()

      let get shared config =
        Shared.locking_set shared.curr_config (Some config);

        let rec loop () =
          match Shared.get shared.partial_result with
          | None -> (
              match Atomic.get shared.closed with
              | `True | `Closed -> assert false
              | `Exn exn ->
                  (* Atomic.set shared.closed `True; *)
                  raise exn
              | _ ->
                  Shared.wait shared.partial_result;
                  loop ())
          | Some pipeline ->
              Shared.set shared.partial_result None;
              pipeline
        in
        Shared.protect shared.partial_result @@ fun () -> loop ()
    ]} *)
