@@ portable

type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Moparser.expr) list;
  evals : Motyper.result;
}

val process : t Shared.t -> Moconfig.t -> unit
val get : t Shared.t -> Moconfig.t -> unit
val typer : t Shared.t -> unit
