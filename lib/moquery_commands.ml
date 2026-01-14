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