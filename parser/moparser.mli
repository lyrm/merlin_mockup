val add_space_before_after : char -> string -> string
val buffer_to_words : string -> string list list @@ portable

type token

val in_par : string list -> string list * string list
val lexer : string list -> token list @@ portable

type expr : immutable_data

val parse_def : token list -> string * expr @@ portable
val eval : (string * int) list -> string * expr -> string * int @@ portable
val rename : (string * int) list ref -> unit @@ portable
val print : (string * int) list ref -> unit
val to_string : (string * int) list ref -> string @@ portable
