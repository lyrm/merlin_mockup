let run_analysis ~access pipeline =
  let typedtree = (Option.get pipeline).Mopipeline.result.typedtree in
  let save = !typedtree in
  Moparser_wrapper.rename ~access typedtree;
  typedtree := save

let analysis hermes _config =
  Hermes.apply_with_access hermes ~f:(fun access _ pipeline ->
      run_analysis ~access pipeline;
      Log.debug 1 "Analysis ran")
