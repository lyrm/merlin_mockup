type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : motyper_result;
}

and motyper_result = (string * int) list ref
