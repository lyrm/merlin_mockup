open! Await
open Moparser

type nonrec expr = expr

let reset_ : unit -> unit = Obj.magic_portable Moparser.reset
let reset ~(access : Hermes.k Capsule.Access.t @ local) () = reset_ ()
let parse = Obj.magic_portable parse
let eval_item = Obj.magic_portable eval_item
let rename_ : env -> unit = Obj.magic_portable rename
let rename ~(access : Hermes.k Capsule.Access.t @ local) env = rename_ env
let to_string_ : env -> string = Obj.magic_portable to_string
let to_string ~(access : Hermes.k Capsule.Access.t @ local) env = to_string_ env
