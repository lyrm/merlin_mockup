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
        Log.debug 1 "Sharing partial result";
        Hermes.send_and_wait hermes Partial_is_available;
        Log.debug 1 "Shared partial result!";
        handle (Handled_effect.continue k () [])
    | Operation (Partial Type_implem, _) -> assert false
  in
  let typer_result =
    handle
      (Motyper.Eff.run (fun handler ->
           Motyper.run config hermes ~handler parsedtree))
  in
  Hermes.merge typer_result ~within:hermes ~f:(fun result _ ->
      Some { source = config.source; parsedtree; result })

let get cfg hermes =
  Hermes.send_and_wait hermes (Config cfg);
  Log.debug 0 "Config changed and received";
  match Hermes.recv_clear hermes with
  | Partial_is_available ->
      Log.debug 0 "Got partial result";
      ()
  | Msg (`Exn exn) ->
      Log.debug 0 "Got exception: %s" (Printexc.to_string exn);
      raise exn
  | _ -> failwith "Unexpected message"

(** Anciennement [domain_typer] *)
let typer hermes =
  let rec loop () =
    try
      Log.debug 1 "Looping";
      match Hermes.recv_clear hermes with
      | Config config ->
          Log.debug 0 "Beginning new config";
          process config hermes;
          let () =
            match config.Moconfig.completion with
            | All -> Hermes.send_and_wait hermes Partial_is_available
            | _ -> (* Partial message is already shared. *) ()
          in
          loop ()
      | Msg `Closing -> Log.debug 0 "Closing"
      | Msg `Cancel -> loop ()
      | _ -> failwith "unexpected msg"
    with
    | Motyper.Cancel_or_closing ->
        Log.debug 0 "Caught Cancel_or_closing.";
        loop ()
    | Motyper.Exception_after_partial exn ->
        Log.debug 0 "Caught an exception after partial result : %s."
          (Printexc.to_string exn);
        loop ()
    | exn ->
        Hermes.send_and_wait hermes (Msg (`Exn exn));
        loop ()
  in
  loop ()
