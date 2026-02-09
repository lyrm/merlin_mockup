let run_analysis pipeline =
  let typedtree = (Option.get pipeline).Mopipeline.result.typedtree in
  let save = !typedtree in
  Moparser_wrapper.rename typedtree;
  typedtree := save

let analysis hermes _config =
  Hermes.apply hermes ~f:(fun _ pipeline -> run_analysis pipeline)
