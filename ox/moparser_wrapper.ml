open Moparser

let buffer_to_words = Obj.magic_portable buffer_to_words

type nonrec expr = expr

let lexer = Obj.magic_portable lexer
let parse_def = Obj.magic_portable parse_def
let eval = Obj.magic_portable eval
let rename = Obj.magic_portable rename
let to_string = Obj.magic_portable to_string

