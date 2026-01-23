@@ portable

type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

val process : Moconfig.t -> t Shared.t -> unit
val get : Moconfig.t -> t Shared.t -> unit
val typer : t Shared.t -> unit
