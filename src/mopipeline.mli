type t = {
  source : string;
  parsedtree : Motyper.parsedtree;
  result : Motyper.result;
}

val get : Moconfig.t -> Motyper.msg Hermes.t -> t option
val domain_typer : Motyper.msg Hermes.t -> unit
