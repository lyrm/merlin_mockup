type t = {
  source : string;
  raw_def : string list list;
  defs : (string * Motool_parser.expr) list;
  evals : Motyper.result;
}

val close_typer : Motyper.shared -> unit @@ portable
val share_exn : Motyper.shared -> exn -> unit @@ portable
val cancel_typer : Motyper.shared -> unit @@ portable
val create_shared : unit -> Motyper.shared
val make : Moconfig.config -> Motyper.shared -> Motyper.result @@ portable
val process : Moconfig.config -> Motyper.shared -> Motyper.result @@ portable
val domain_typer : Motyper.shared @ contended -> unit -> unit @@ portable
val get : Motyper.shared -> Moconfig.config -> t option
