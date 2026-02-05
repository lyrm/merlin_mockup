open Debug

(** [run_analysis] *)
let run_analysis partial_pipeline _config =
  let typedtree = partial_pipeline.Mopipeline.result.typedtree in
  let save = !typedtree in
  Moparser.rename typedtree;
  if print_partial then begin
    Utils.log 0 "Partial result:";
    Moparser.print typedtree
  end;
  partial_pipeline.Mopipeline.result.typedtree := save

(** [analysis] *)
let analysis hermes partial_pipeline config =
  (* Main domain waits for the typer domain to finish its analysis *)
  Hermes.protect hermes (fun () ->
      run_analysis partial_pipeline config;
      Hermes.signal hermes)
