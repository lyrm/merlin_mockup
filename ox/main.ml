open! Await

(** Anciennement [domain_typer] *)
let typer (shared : Mopipeline.t Shared.t) =
  let rec loop () =
    try
      match Shared.recv_clear shared with
      | Config config ->
          let () = Mopipeline.process shared config in
          Shared.send_and_wait shared Partial_is_available;
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
let run_analysis shared _config =
  Shared.protected_apply shared (fun _msg ->
      Option.iter (fun pipeline ->
          let evals = pipeline.Mopipeline.evals in
          let save = !evals in
          Moparser_wrapper.rename evals;
          pipeline.evals := save))

(** [New_merlin.run] ou [New_commands.run] *)
let run shared config =
  Shared.send_and_wait shared (Msg `Cancel);
  Mopipeline.get shared config;
  run_analysis shared config

(** [Ocaml_merlin_server.main] *)
let () =
  let shared = Shared.create () in
  let module Scheduler = Parallel_scheduler in
  let scheduler = Scheduler.create () in
  Scheduler.parallel scheduler ~f:(fun par ->
      let #((), ()) =
        Parallel.fork_join2 par
          (fun _ ->
            Server.listen ~handle:(fun req ->
                run shared req;
                let resp =
                  Await_blocking.with_await Terminator.never ~f:(fun await ->
                      Mutex.with_key await (Shared.mutex shared) ~f:(fun key ->
                          Capsule.Expert.Key.access key ~f:(fun access ->
                              let res =
                                Capsule.Data.unwrap ~access Motyper.res
                              in
                              {
                                Modes.Aliased.aliased =
                                  Moparser_wrapper.to_string
                                    (ref (List.rev !res));
                              })))
                in
                resp.aliased);
            Shared.send_and_wait shared (Msg `Closing))
          (fun _ -> typer shared)
      in
      ());
  Scheduler.stop scheduler
