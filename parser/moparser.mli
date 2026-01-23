type typedtree = typed_item list
and typed_item = string * int

type parsedtree = parsed_item list
and parsed_item = string * expr
and expr = Var of string | Int of int | Binop of (op * expr * expr)
and op = Div | Add | Sub | Mul

type env = (string * int) list ref

val parse : string -> parsedtree
val eval_item : (string * int) list -> parsed_item -> typed_item
val rename : env -> unit
val print : env -> unit
val to_string : env -> string
