open! Await

module Mopipeline = struct
  include Mopipeline

  let process shared config =
    let rec handle = function
      | Motyper.Eff.Value evals -> evals
      | Exception exn -> raise exn
      | Operation (Partial (Run _), k) ->
          Shared.send_and_wait shared Partial_is_available;
          handle (Effect.continue k () [])
      | Operation (Partial (TI _), _) -> assert false
    in
    handle
      (Motyper.Eff.run (fun handler ->
           let raw_def = Moparser.buffer_to_words config.Moconfig.source in
           let defs =
             List.map Moparser.lexer raw_def |> List.map Moparser.parse_def
           in
           Motyper.run handler shared defs config))

  let make config shared = process config shared

  let get shared cfg =
    Shared.send_and_wait shared (Config cfg);
    Shared.protected_update shared (fun _msg ->
        Option.map (fun pipeline ->
            {
              source = cfg.source;
              raw_def = [];
              defs = [];
              evals = pipeline.evals;
            }));
    match Shared.recv_clear shared with
    | Partial_is_available -> ()
    | Msg (`Exn exn) -> raise exn
    | _ -> failwith "Unexpected message"
end

(** Anciennement [domain_typer] *)
let typer shared =
  let rec loop () =
    try
      match Shared.recv_clear shared with
      | Config cfg ->
          Shared.protected_apply shared (fun _msg _pipeline ->
              let () = Mopipeline.make shared cfg in
              match cfg.completion with
              | All -> Shared.send_and_wait shared Partial_is_available
              | _ -> (* Already shared *) ());
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
          Moparser.rename evals;
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
  let socket_fname = Sys.argv.(1) in
  Scheduler.parallel scheduler ~f:(fun par ->
      let #((), ()) =
        Parallel.fork_join2 par
          (fun _ ->
            Server.listen ~socket_fname ~handle:(fun req ->
                run shared req;
                let res = !(Obj.magic_uncontended Motyper.res) |> List.rev in
                Moparser.to_string (ref res));
            Shared.send_and_wait shared (Msg `Closing))
          (fun _ -> typer shared)
      in
      ());
  Scheduler.stop scheduler
