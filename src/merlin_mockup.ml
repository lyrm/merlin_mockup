exception Closing

(** [New_merlin.run] ou [New_commands.run] *)
let run config (hermes : Mopipeline.t option Hermes.t) =
  Hermes.send_and_wait hermes (Msg `Cancel);
  Log.debug 0 "Start to processing request %S" config.Moconfig.source;
  Mopipeline.get config hermes;
  Moquery_commands.analysis hermes config

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
        (try
           Server.listen ~handle:(fun req ->
               match req with
               | Server.Close -> raise Closing
               | Server.Config config ->
                   run config hermes;
                   Hermes.apply hermes ~f:(fun _ pipeline ->
                       let evals =
                         match pipeline with
                         | None -> failwith "No pipeline found (main)"
                         | Some p -> p.Mopipeline.result
                       in
                       let res = Motyper.(evals.typedtree) in
                       Moparser_wrapper.to_string (ref (List.rev !res))))
         with
        | Closing ->
            Log.debug 0 "Closing requested received.";
            Hermes.send_and_wait hermes (Msg `Closing)
        | _ ->
            Log.debug 0 "Server thread exiting with exception";
            Hermes.send_and_wait hermes (Msg `Closing));
        Log.debug 0 "Exiting";

        Atomic.incr counter;
        Log.debug 0 "counter = %d" (Atomic.get counter))
      ()
  in

  let* () =
    Multicore.spawn
      (fun () ->
        Mopipeline.typer hermes;
        Log.debug 0 "Exiting.";

        Atomic.incr counter;
        Log.debug 0 "counter = %d" (Atomic.get counter))
      ()
  in

  (* Waiting for all domains to finish *)
  while Atomic.get counter <> 2 do
    Thread.yield ()
  done;
  Log.debug 0 "The end"

(* The rest of the file is just a copy of the original merlin.ml, with some minor edits to make it compile *)
