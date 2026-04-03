@@ portable

open! Await
open Moparser

type expr : immutable_data = Moparser.expr

(** Functions that don't touch mutable state — no access required. *)
val reset : access:Hermes.k Capsule.Access.t -> unit -> unit
val parse : string -> parsedtree
val eval_item : (string * int) list -> parsed_item -> string * int

(** Functions that touch mutable state (env = typedtree ref).
    Require a [Hermes.k Capsule.Access.t] to ensure the caller holds
    the mutex. *)
val rename : access:Hermes.k Capsule.Access.t -> env -> unit
val to_string : access:Hermes.k Capsule.Access.t -> env -> string
