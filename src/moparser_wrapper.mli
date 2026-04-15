@@ portable

open! Await
open Moparser

type expr : immutable_data = Moparser.expr

val reset : access:Hermes.k Capsule.Access.t @ local -> unit -> unit

val parse : string -> parsedtree

val eval_item : (string * int) list -> parsed_item -> string * int

val rename : access:Hermes.k Capsule.Access.t @ local -> env -> unit

val to_string : access:Hermes.k Capsule.Access.t @ local -> env -> string
