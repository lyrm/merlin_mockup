@@ portable

open Moparser

val buffer_to_words : string -> string list list

type expr : immutable_data = Moparser.expr

val lexer : string list -> token list
val parse_def : token list -> string * expr
val eval : (string * int) list -> string * expr -> string * int
val rename : (string * int) list ref -> unit
val to_string : (string * int) list ref -> string
