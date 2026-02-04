open! Await

(** [Query_commands.run] et [Query_commands.run_analysis] *)
let run_analysis hermes _config =
  Hermes.apply hermes ~f:(fun _ pipeline ->
      let typedtree = (Option.get pipeline).Mopipeline.result.typedtree in
      let save = !typedtree in
      Moparser_wrapper.rename typedtree;
      typedtree := save)

(** [New_merlin.run] ou [New_commands.run] *)
let run config (hermes : Mopipeline.t option Hermes.t) =
  Hermes.send_and_wait hermes (Msg `Cancel);
  Mopipeline.get config hermes;
  run_analysis hermes config;
  prerr_endline "analysis ran"

let () =
  let hermes = Hermes.create (fun () -> None) in
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
            run req hermes;
            prerr_endline "Ran";
            Hermes.apply hermes ~f:(fun _ pipeline ->
                let evals = (Option.get pipeline).Mopipeline.result in
                let res = Motyper.(evals.typedtree) in
                Moparser_wrapper.to_string (ref (List.rev !res))));
        Hermes.send_and_wait hermes (Msg `Closing);
        Atomic.incr counter)
      ()
  in
  let* () =
    Multicore.spawn
      (fun () ->
        Mopipeline.typer hermes;
        Atomic.incr counter)
      ()
  in
  while Atomic.get counter <> 2 do
    Domain.cpu_relax ()
  done
