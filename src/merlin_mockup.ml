exception Closing

(** [run] = New_merlin.run ou New_commands.run *)
let run =
  let req_count = ref 0 in
  fun config hermes ->
    Hermes.send_and_wait hermes (Motyper.Msg `Cancel);
    incr req_count;
    let result = Mopipeline.get config hermes in
    Utils.log 0 "Request nb %i - Beginning analysis" !req_count;
    Option.iter (fun r -> Moquery_commands.analysis hermes r config) result;
    result

(** [main] = Ocaml_merlin_server.main *)
let main () =
  let hermes = Hermes.create () in
  Utils.log 0 "Spawning typer";
  let domain_typer = Domain.spawn (fun () -> Mopipeline.domain_typer hermes) in
  begin
    try
      Server.listen ~handle:(fun req ->
          match req with
          | Server.Close -> raise Closing
          | Config config ->
              let items =
                match run config hermes with
                | None -> failwith "No pipeline found (main)"
                | Some pipeline -> pipeline.Mopipeline.result.typedtree
              in
              Moparser.to_string (ref (List.rev !items)));
      Domain.join domain_typer
    with
    | Closing ->
        Utils.log 0 "Closing requested received.";
        Hermes.send_and_wait hermes (Motyper.Msg `Close)
    | _ ->
        Utils.log 0 "Server thread exiting with exception";
        Hermes.send_and_wait hermes (Motyper.Msg `Close)
  end;
  Utils.log 0 "The end"

let () = main ()
