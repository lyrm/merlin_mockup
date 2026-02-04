@@ portable

type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

val get : Moconfig.t -> t option Hermes.t -> unit
val typer : t option Hermes.t -> unit
