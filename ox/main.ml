open! Await

(** Anciennement [domain_typer] *)
let typer shared =
  let rec loop () =
    try
      prerr_endline "Typer: looping";
      match Shared.recv_clear shared with
      | Config config ->
          prerr_endline "Typer: received config";
          let () = Mopipeline.process shared config in
          prerr_endline "Typer: pipeline processed";
          Shared.send_and_wait shared Partial_is_available;
          prerr_endline "Typer: partial is available";
          loop ()
      | Msg `Closing -> ()
      | Msg `Cancel -> loop ()
      | _ -> failwith "unexpected msg"
    with
    | Motyper.(Cancel_or_closing | Exception_after_partial _) -> loop ()
    | exn ->
        Shared.send_and_wait shared (Msg (`Exn exn));
        loop ()
  in
  loop ()

(** [Query_commands.run] et [Query_commands.run_analysis] *)
(* let run_analysis shared _config =
  Shared.protected_apply shared (fun _msg ->
      Option.iter (fun pipeline ->
          let evals = pipeline.Mopipeline.evals in
          let save = !evals in
          Moparser_wrapper.rename evals;
          pipeline.evals := save)) *)

(** [New_merlin.run] ou [New_commands.run] *)
let run shared config =
  Shared.send_and_wait shared (Msg `Cancel);
  prerr_endline "Main: send cancel";
  Mopipeline.get shared config

(* ;
  run_analysis shared config *)

let () =
  let shared = Shared.create () in
  let counter = Atomic.make 0 in
  let ( let* ) spawn_result f =
    match spawn_result with
    | Multicore.Spawned -> f ()
    | Failed ((), exn, backtrace) ->
        Format.eprintf "%s: %s\n%!" (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string backtrace)
  in
  let* () =
    Multicore.spawn
      (fun () ->
        Server.listen ~handle:(fun req ->
            prerr_endline req.source;
            run shared req;
            prerr_endline "Ran";
            let resp =
              Await_blocking.with_await Terminator.never ~f:(fun await ->
                  Mutex.with_key await (Shared.mutex shared) ~f:(fun key ->
                      Capsule.Expert.Key.access key ~f:(fun access ->
                          let res = Capsule.Data.unwrap ~access Motyper.res in
                          {
                            Modes.Aliased.aliased =
                              Moparser_wrapper.to_string (ref (List.rev !res));
                          })))
            in
            prerr_endline @@ "Respond: " ^ resp.aliased;
            resp.aliased);
        Shared.send_and_wait shared (Msg `Closing);
        Atomic.incr counter)
      ()
  in
  let* () =
    Multicore.spawn
      (fun () ->
        typer shared;
        Atomic.incr counter)
      ()
  in
  while Atomic.get counter <> 2 do
    Domain.cpu_relax ()
  done
