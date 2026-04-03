@@ portable

open! Await
open Moparser

type expr : immutable_data = Moparser.expr

val reset : access:Hermes.k Capsule.Access.t @ local -> unit -> unit
(** Functions that don't touch mutable state — no access required. *)

val parse : string -> parsedtree
val eval_item : (string * int) list -> parsed_item -> string * int

val rename : access:Hermes.k Capsule.Access.t @ local -> env -> unit
(** Functions that touch mutable state (env = typedtree ref). Require a
    [Hermes.k Capsule.Access.t] to ensure the caller holds the mutex. *)

val to_string : access:Hermes.k Capsule.Access.t @ local -> env -> string
