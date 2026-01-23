open! Await

(** [Query_commands.run] et [Query_commands.run_analysis] *)
(* let run_analysis shared _config =
  Shared.protected_apply shared (fun _msg ->
      Option.iter (fun pipeline ->
          let evals = pipeline.Mopipeline.evals in
          let save = !evals in
          Moparser_wrapper.rename evals;
          pipeline.evals := save)) *)

(** [New_merlin.run] ou [New_commands.run] *)
let run config shared =
  Shared.send_and_wait shared (Msg `Cancel);
  prerr_endline "Main: send cancel";
  Mopipeline.get config shared

(* ;
  run_analysis shared config *)

let () =
  let shared = Shared.create (fun () -> None) in
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
            run req shared;
            prerr_endline "Ran";
            Shared.apply shared ~f:(fun _ { Mopipeline.evals; _ } ->
                let res = Motyper.(evals.typedtree) in
                Moparser_wrapper.to_string (ref (List.rev !res))));
        Shared.send_and_wait shared (Msg `Closing);
        Atomic.incr counter)
      ()
  in
  let* () =
    Multicore.spawn
      (fun () ->
        Mopipeline.typer shared;
        Atomic.incr counter)
      ()
  in
  while Atomic.get counter <> 2 do
    Domain.cpu_relax ()
  done
