open Moparser

type nonrec expr = expr

let parse = Obj.magic_portable parse
let eval_item = Obj.magic_portable eval_item
let rename = Obj.magic_portable rename
let to_string = Obj.magic_portable to_string
