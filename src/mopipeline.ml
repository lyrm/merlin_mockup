type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

let process config (hermes : t option Hermes.t) =
  let parsedtree = Moparser_wrapper.parse config.Moconfig.source in
  let rec handle = function
    | Motyper.Eff.Value evals -> evals
    | Exception exn -> raise exn
    | Operation (Partial Run, k) ->
        Hermes.send_and_wait hermes Partial_is_available;
        handle (Handled_effect.continue k () [])
    | Operation (Partial Type_implem, _) -> assert false
  in
  let typer_result =
    handle
      (Motyper.Eff.run (fun handler ->
           Motyper.run config hermes ~handler parsedtree))
  in
  prerr_endline "Typer: typer has ran";
  Hermes.merge typer_result ~within:hermes ~f:(fun result _ ->
      Some { source = config.source; parsedtree; result })

let get cfg hermes =
  Hermes.send_and_wait hermes (Config cfg);
  prerr_endline "Main: send config";
  match Hermes.recv_clear hermes with
  | Partial_is_available ->
      prerr_endline "Main: partial is available !";
      ()
  | Msg (`Exn exn) -> raise exn
  | _ -> failwith "Unexpected message"

(** Anciennement [domain_typer] *)
let typer hermes =
  let rec loop () =
    try
      prerr_endline "Typer: looping";
      match Hermes.recv_clear hermes with
      | Config config ->
          prerr_endline "Typer: received config";
          let () = process config hermes in
          prerr_endline "Typer: pipeline processed";
          let () =
            match config.Moconfig.completion with
            | All ->
                prerr_endline "Typer: partial is available";
                Hermes.send_and_wait hermes Partial_is_available
            | _ -> (* Partial message is already shared. *) ()
          in
          loop ()
      | Msg `Closing -> ()
      | Msg `Cancel -> loop ()
      | _ -> failwith "unexpected msg"
    with
    | Motyper.(Cancel_or_closing | Exception_after_partial _) -> loop ()
    | exn ->
        Hermes.send_and_wait hermes (Msg (`Exn exn));
        loop ()
  in
  loop ()
