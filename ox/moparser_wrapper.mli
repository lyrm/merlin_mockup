@@ portable

open Moparser

type expr : immutable_data = Moparser.expr

val parse : string -> parsedtree
val eval_item : (string * int) list -> parsed_item -> string * int
val rename : env -> unit
val to_string : env -> string
