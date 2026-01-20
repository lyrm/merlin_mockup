@@ portable

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : Motyper.result;
}

val process : t Shared.packed -> Moconfig.t -> unit
val get : 'a Shared.packed -> Moconfig.t -> unit
