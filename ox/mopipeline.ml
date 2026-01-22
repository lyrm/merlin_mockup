type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : Motyper.result;
}

let process shared config =
  let raw_def = Moparser_wrapper.buffer_to_words config.Moconfig.source in
  let defs =
    List.map Moparser_wrapper.lexer raw_def
    |> List.map Moparser_wrapper.parse_def
  in
  let evals =
    Shared.project shared ~f:(fun o ->
        Option.map (fun pipeline -> pipeline.evals) o)
  in
  prerr_endline "Typer: evals got";
  Motyper.run evals defs config;
  prerr_endline "Typer: typer has ran"

(*; Shared.merge evals ~within:shared ~f:(fun evals ~within:_ ->
      { source = config.source; raw_def; defs; evals = Option.get evals }) *)

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
  prerr_endline "Main: send config";
  match Shared.recv_clear shared with
  | Partial_is_available ->
      prerr_endline "Main: partial is available !";
      ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"
