type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

let process config shared =
  let parsedtree = Moparser_wrapper.parse config.Moconfig.source in
  prerr_endline "Typer: evals got";
  let evals = Motyper.run config shared parsedtree in
  prerr_endline "Typer: typer has ran";
  Shared.merge evals ~within:shared ~f:(fun result _ ->
      { source = config.source; parsedtree; result })

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

let get cfg shared =
  Shared.send_and_wait shared (Config cfg);
  prerr_endline "Main: send config";
  match Shared.recv_clear shared with
  | Partial_is_available ->
      prerr_endline "Main: partial is available !";
      ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"

(** Anciennement [domain_typer] *)
let typer shared =
  let rec loop () =
    try
      prerr_endline "Typer: looping";
      match Shared.recv_clear shared with
      | Config config ->
          prerr_endline "Typer: received config";
          let () = process config shared in
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
