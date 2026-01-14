open Debug

(** [run_analysis] *)
let run_analysis partial_pipeline _config =
  let evals = partial_pipeline.Mopipeline.evals in
  let save = !evals in
  Motool_parser.rename evals;
  if print_partial then begin
    Utils.log 0 "Partial result:";
    Motool_parser.print evals
  end;
  partial_pipeline.Mopipeline.evals := save

(** [analysis] *)
let analysis shared partial_pipeline config =
  (* Main domain signals it wants the lock  *)
  Atomic.set shared.Motyper.waiting true;

  (* Main domain waits for the typer domain to finish its analysis *)
  Moshared.protect shared.msg (fun () ->
      Atomic.set shared.waiting false;
      run_analysis partial_pipeline config;

      (*
run partie pure de l'analyse en parallèle
et mettre sous mutex que la mutation finale pour simuler comportement de la plupart des requêtes de l'analyse
*)
      Moshared.signal shared.msg)

(** [run] = New_merlin.run ou New_commands.run *)
let run =
  let req_count = ref 0 in
  fun shared config ->
    Mopipeline.cancel_typer shared;
    incr req_count;
    let result = Mopipeline.get shared config in
    Utils.log 0 "Request nb %i - Beginning analysis" !req_count;
    Option.iter (fun r -> analysis shared r config) result

(** [main] = Ocaml_merlin_server.main *)
let main () =
  let shared = Mopipeline.create_shared () in
  Utils.log 0 "Spawning typer";

  let domain_typer = Domain.spawn (fun () -> Mopipeline.domain_typer shared) in
  Server.listen ~socket_fname:Sys.argv.(1) ~handle:(fun req ->
      run shared req;
      let res = !Motyper.res |> List.rev in
      Motool_parser.to_string (ref res));
  Mopipeline.close_typer shared;
  Domain.join domain_typer;

  if print_last then begin
    Utils.log 0 "Last result :";
    Motool_parser.print (ref (List.rev !Motyper.res))
  end

(* Clean up the socket once the server is shutdown. *)
let () =
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> Unix.unlink Sys.argv.(1)))

let () = main ()

(* Things in Merlin that might not work well
- ctype
- src/analysis
- local_store
- unify_gadt -> in lock (with snapshot /backtrack around )
*)
