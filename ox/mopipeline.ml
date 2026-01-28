type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

let process config (shared : t option Shared.t) =
  let parsedtree = Moparser_wrapper.parse config.Moconfig.source in
  let rec handle = function
    | Motyper.Eff.Value evals -> evals
    | Exception exn -> raise exn
    | Operation (Partial Run, k) ->
        Shared.send_and_wait shared Partial_is_available;
        handle (Effect.continue k () [])
    | Operation (Partial Type_implem, _) -> assert false
  in
  let typer_result =
    handle
      (Motyper.Eff.run (fun handler ->
           Motyper.run config shared ~handler parsedtree))
  in
  prerr_endline "Typer: typer has ran";
  Shared.merge typer_result ~within:shared ~f:(fun result _ ->
      Some { source = config.source; parsedtree; result })

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
