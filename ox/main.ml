open! Await

let process shared config =
  let raw_def = Moparser.buffer_to_words config.Moconfig.source in
  let defs = List.map Moparser.lexer raw_def |> List.map Moparser.parse_def in
  let open Stdlib.Effect.Deep in
  match_with (Motyper.run shared defs) config
    {
      retc = (fun evals -> evals);
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Motyper.Partial (Run evals) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Shared.send_and_wait shared Partial_is_available;
                  continue k ())
          | _ -> None);
    }

let typer shared =
  let rec loop () =
    (* TODO: catch exceptions *)
    match Shared.recv_clear shared with
    | Config cfg ->
        Shared.protected_apply shared (fun _msg _pipeline -> process shared cfg);
        loop ()
    | Msg `Closing -> ()
    | Msg `Cancel -> loop ()
    | _ -> failwith "unexpected msg"
  in
  loop ()

(** [Mopipeline.get]: *)
let get shared cfg =
  Shared.send_and_wait shared (Config cfg);
  Shared.protected_update shared (fun _msg _pipeline ->
      Some { source = cfg.source; raw_def = []; defs = []; evals = ref [] });
  match Shared.recv_clear shared with
  | Partial_is_available -> ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"

(** [New_merlin.run] ou [New_commands.run] *)
let run shared config =
  Shared.send_and_wait shared (Msg `Cancel);
  get shared config;
  Shared.protected_apply shared (fun _msg _pipeline ->
      (* TODO: faire analyse *) ())

(** [main] = Ocaml_merlin_server.main *)
let main () =
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
                (* TODO: récupérer résultat et le print *)
                (* let res = !Motyper.res |> List.rev in
                   Motool_parser.to_string (ref res) *)
                "TODO");
            Shared.send_and_wait shared (Msg `Closing))
          (fun _ -> typer shared)
      in
      ());
  Scheduler.stop scheduler

let () = main ()
