(** Some config*)
let len = 100

(** config is the index at which the computation stop *)
(* let config = 12 *)

let debug_lvl = 1

(** Mutable state *)
let shared_value = Array.make len 0

(** Mpipeline *)
module Mpipeline = struct
  type t = int * int array

  (* Possible communication between the two domains: 
  + From Main to Typer :
    - request is canceled
    - merlin is closing
    - is waiting for the lock

  + From Typer to Main :
    - caught an exception 
*)

  (* Do we need a complete result shared variables ? 
  NO :
    1- if the main domain need a complete result it is going to be in partial 
      result anyway
    2- the purpose of complete the typing if for the cash
  YES :
    1- We need a lock to protect it -> would a lock on the partial result be 
      enough ?
*)

  type mess_main = [ `Waiting | `Empty | `Cancel | `Closing ]
  type mess_typer = [ `Empty | `Exn of exn ]

  type shared = {
    mess_main : mess_main Atomic.t;
    mess_typer : mess_typer Atomic.t;
    config : int option Shared.t;
    partial : t option Shared.t;
    result : t option Shared.t;
  }

  exception Bug of int
  exception Closing
  exception Cancel

  (** [create_shared] *)
  let create_shared () =
    {
      mess_main = Atomic.make `Empty;
      mess_typer = Atomic.make `Empty;
      config = Shared.create None;
      partial = Shared.create None;
      result = Shared.create None;
    }

  let send_msg mess new_msg signal_on =
    (* CAS could be replaced by `set` here *)
    if Atomic.compare_and_set mess `Empty new_msg then
      while Atomic.get mess == new_msg do
        Shared.signal signal_on
      done
    else failwith "send_msg: should not happen."

  (** [closing]: called by the main domain *)
  let closing shared = send_msg shared.mess_main `Closing shared.config

  (** [share_exn]: called by the typer domain *)
  let share_exn shared exn =
    send_msg shared.mess_typer (`Exn exn) shared.partial

  (** [cancel] is called by the main domain *)
  let cancel (shared : shared) = send_msg shared.mess_main `Cancel shared.config

  (** [typer] *)
  let typer config shared =
    let string_domain = Utils.string_domain () in
    let first = ref true in
    for ind = 0 to len - 1 do
      if debug_lvl > 1 && ind mod 10 = 0 then
        Format.printf "%sTyping step %d in %d\n%!" string_domain ind len;

      (match Atomic.get shared.mess_main with
      | `Waiting ->
          if debug_lvl > 0 then
            Format.printf "%sGive lock to main for analysis.\n%!" string_domain;
          (* Active waiting seems ok here as the whole purpose of the typer is
             to type as fast as possible. Thus its waiting time should be as 
             short as possible. *)
          while Atomic.get shared.mess_main == `Waiting do
            Domain.cpu_relax ()
          done
      | `Closing ->
          if debug_lvl > 0 then Format.printf "%sClosing.\n%!" string_domain;
          raise Closing
      | `Cancel ->
          if debug_lvl > 0 then
            Format.printf "%sRequest is cancelled.\n%!" string_domain;
          Atomic.set shared.mess_main `Empty;
          raise Cancel
      | `Empty -> ());
      try
        (* Finally typing *)
        Shared.protect shared.result (fun () ->
            let res = ind in
            if Random.int 10 == 0 then raise (Bug config);
            shared_value.(ind) <- res + shared_value.(ind);
            if ind >= config && !first then (
              (* [first] enables to put >= instead of = which is going to be 
              useful with the cash (typing can begin after the line we want to 
              stop if the beginning is already in cash)*)
              first := false;
              Shared.locking_set shared.partial (Some (config, shared_value)));
            (* Simulating some additional work*)
            Utils.stupid_work () |> ignore)
      with exn ->
        if ind >= config then (* TODO : log the exception *) () else raise exn
    done

  (** [make] *)
  let make config shared =
    let string_domain = Utils.string_domain () in
    if debug_lvl > 0 then
      Format.printf "%sBegin config %d\n%!" string_domain config;
    typer config shared;
    if debug_lvl > 0 then
      Format.printf "%sFinish typing config %d\n%!" string_domain config

  (** [domain_typer] *)
  let domain_typer shared () =
    let string_domain = Utils.string_domain () in

    let rec loop () =
      if debug_lvl > 0 then Format.printf "%sLooping\n%!" string_domain;

      match Atomic.get shared.mess_main with
      | `Closing ->
          if debug_lvl > 0 then Format.printf "%sClosing\n%!" string_domain;
          Atomic.set shared.mess_main `Empty
      | `Waiting ->
          while Atomic.get shared.mess_main == `Waiting do
            Domain.cpu_relax ()
          done;
          loop ()
      | `Cancel ->
          Atomic.set shared.mess_main `Empty;
          loop ()
      | `Empty -> (
          match Shared.get shared.config with
          | None ->
              if debug_lvl > 0 then
                Format.printf "%sWaiting for a config\n%!" string_domain;
              Shared.wait shared.config;
              loop ()
          | Some config ->
              Shared.set shared.config None;
              (try
                 make config shared;
                 Shared.locking_set shared.result (Some (config, shared_value))
               with
              | Closing -> ()
              | Cancel -> ()
              | Bug _ as exn -> share_exn shared exn);
              loop ())
    in
    if debug_lvl > 0 then Format.printf "%sBegin\n%!" string_domain;
    Shared.protect shared.config (fun () -> loop ())

  (** [get] *)
  let get shared config =
    let string_domain = Utils.string_domain () in
    if debug_lvl > 0 then
      Format.printf "%sWait for lock to change config.\n%!" string_domain;

    Shared.locking_set shared.config (Some config);

    if debug_lvl > 0 then Format.printf "%sConfig changed.\n%!" string_domain;

    let rec loop () =
      let critical_section () =
        match Shared.get shared.partial with
        | None -> (
            match Atomic.get shared.mess_typer with
            | `Empty ->
                if debug_lvl > 0 then
                  Format.printf "%sWaiting for a result\n%!" string_domain;
                Shared.wait shared.partial;
                `Retry
            | `Exn (Bug buggy_config) ->
                if debug_lvl > 0 then
                  Format.printf "%sWitness the exception of config %d.\n%!"
                    string_domain buggy_config;
                Atomic.set shared.mess_typer `Empty;
                (* TODO : This needs to change as for now we ignore the raise exn *)
                if buggy_config == config then `Result None else `Retry
            | _ -> failwith "Uncaught exception")
        | Some ((_rconfig, _) as pipeline) ->
            if debug_lvl > 0 then
              Format.printf "%sSome partial result \n%!" string_domain;

            (* if rconfig != config then (
              Format.printf
                "%sMismatching config %d / curr : %d (curr_config = %s)\n%!"
                string_domain rconfig config
                (Option.fold ~none:"None" ~some:string_of_int
                   (Shared.get shared.curr_config));
              failwith "problem"); *)
            Shared.set shared.partial None;
            `Result (Some pipeline)
      in
      match Shared.protect shared.partial critical_section with
      | `Retry -> loop ()
      | `Result r -> r
    in
    loop ()

  (* Not sure which implem is better. I prefer protect over lock/unlock but the use of polymorphic variants can be bad for perf. *)
  (* let get shared config =
  Shared.locking_set shared.config (Some config);

  let rec loop () =
    let critical_section () =
      Shared.lock shared.partial_result;
      match Shared.get shared.partial_result with
      | None -> (
        match Atomic.get shared.mess_typer with
        | `Empty ->
          Shared.wait shared.partial_result;
          Shared.unlock shared.partial_result;
          loop ()
        | `Exn exn ->
          Atomic.set shared.mess_typer `Empty;
          Shared.unlock shared.partial_result;
          raise exn)
      | Some pipeline ->
        Shared.set shared.partial_result None;
        Shared.unlock shared.partial_result;
        pipeline
    in
    critical_section ()
  in
  loop () *)
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
  if debug_lvl > 0 then Format.printf "%sBegin analysis.\n%!" string_domain;

  if config_typer <> config then failwith "impossible ?";
  (* Main domain signals it wants the lock  *)
  if Atomic.compare_and_set shared.mess_main `Empty `Waiting then (
    if debug_lvl > 0 then
      Format.printf "%sWaiting to get the lock.\n%!" string_domain;

    Shared.protect shared.result (fun () ->
        (* The write on mess_main needed 
            to happen in the lock to ensure the main domain got it, before 
            releasing the typer domain of its active wait *)
        Atomic.set shared.mess_main `Empty;
        run_analysis partial_pipeline config))
  else
    (* This can happen when the typer domain found an exception *)
    failwith "To debug .";
  if debug_lvl > 0 then Format.printf "%sAnalysis finished.\n%!" string_domain

(** [random_config] *)
let random_config () = Random.int 5

(** [run] = New_merlin.run ou New_commands.run*)
let run shared count_max =
  let string_domain = Utils.string_domain () in

  let rec do_one_request prev count =
    if count = count_max then ()
    else
      let config = random_config () in
      if debug_lvl > 0 then
        Format.printf
          "*****************\n%sNew Request number = %d with config = %d.\n%!"
          string_domain count config;
      if prev = config then Mpipeline.cancel shared;
      (* Reinit shared is required : that an issue : when should we reinit ?
      
        -> the typer should be the one that do it 
        -> would require a mess_main to ensure main witness it (but does it need to ?)
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
  if debug_lvl > 0 then
    Format.printf "%sSpawning typer\n%!" (Utils.string_domain ());

  let domain_typer = Domain.spawn @@ Mpipeline.domain_typer shared in

  let _ = run shared 100 in

  if debug_lvl > 0 then
    Format.printf "%sRun finished\n%!" (Utils.string_domain ());

  Mpipeline.closing shared;
  Domain.join domain_typer;
  ()
(* Utils.print_arr_int shared_value *)

let () = main ()

(* Bug issueLSP695 *)

(*
  Is there an issue depending on when an exception is raised by the typer domain ?

  - before a partial result has been returned : No, the main domain is located 
  before the write on `curr_config` and the wait on `partial_result`.  

  - after a partial result has been returned : the main domain can be anywhere. 
  In particular, it could be already processing a new request / config. However, 
  it only becomes aware of the issue when it looks at the `mess_main`. But then 
  the typer is waiting for ACK to continue. So it should be has soon as possible. 
  -> It is not an issue if the reading of `mess_main` is always able to manage the 
    possible caught exception and if it happens often enough. 

*)

(* Things in Merlin that might not work well
- ctype
- src/analysis
- local_store
- unify_gadt -> in lock (with snapshot /backtrack around )
*)
