val add_space_before_after : char -> string -> string
val buffer_to_words : string -> string list list

type token

val in_par : string list -> string list * string list
val lexer : string list -> token list

type op = Div | Add | Sub | Mul
type expr = Var of string | Int of int | Binop of (op * expr * expr)

val parse_def : token list -> string * expr
val eval : (string * int) list -> string * expr -> string * int
val rename : (string * int) list ref -> unit
val print : (string * int) list ref -> unit
val to_string : (string * int) list ref -> string
