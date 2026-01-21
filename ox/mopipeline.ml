type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : Motyper.result;
}

let merge shared typer_result f =
  let open Await in
  Await_blocking.with_await Terminator.never ~f:(fun await ->
      Mutex.with_key await (Shared.mutex shared) ~f:(fun key ->
          Capsule.Expert.Key.access key ~f:(fun access ->
              let pipeline = Capsule.Data.unwrap ~access (Shared.data shared) in
              let result =
                Capsule.Data.unwrap ~access (Shared.data typer_result)
              in
              pipeline := Some (f (Option.get !pipeline) (Option.get !result)))))

let (process @ portable) shared config =
  let raw_def = Moparser_wrapper.buffer_to_words config.Moconfig.source in
  let defs =
    List.map Moparser_wrapper.lexer raw_def
    |> List.map Moparser_wrapper.parse_def
  in
  let evals =
    Shared.project shared ~f:(fun o ->
        Option.map (fun pipeline -> pipeline.evals) o)
  in
  Motyper.run evals defs config;
  merge shared evals (fun _pipeline evals ->
      { source = config.source; raw_def; defs; evals })

(* let rec handle = function
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
           Motyper.run handler shared defs config)) *)

let get shared cfg =
  Shared.send_and_wait shared (Config cfg);
  match Shared.recv_clear shared with
  | Partial_is_available -> ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"
