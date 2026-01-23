@@ portable

open! Await

type 'a t : value mod contended portable
type k
type mutex = k Mutex.t

val create : (unit -> 'a) @ portable -> 'a t
val send_and_wait : 'a t -> Msg.t -> unit
val recv_clear : 'a t -> Msg.t

val create_from :
  'a t -> ('b, k) Capsule_expert.Data.t -> f:('b -> 'c) @ portable -> 'c t

val map : 'a t -> f:('a -> 'b) @ portable -> 'b t
(** [map t ~f] applies [f] to the data inside [t] under the protection of its
    mutex and returns a new shared data structure with the same mutex, condition
    and message. *)

val apply :
  ('b : immutable_data). 'a t -> f:(Msg.t option -> 'a -> 'b) @ portable -> 'b
(** [apply t ~f] applies [f] to the message and data inside [t] under the
    protection of its mutex and returns the result. *)

(* val set : 'a t -> f:(Msg.t option -> 'a option) @ portable -> unit  *)
(** [set t f] sets the data inside [t] to [f msg] where [msg] is the message
    currently stored inside [t], all under the protection of its mutex. *)

val apply_with_capsule :
  ('c : immutable_data) ('b : value mod portable).
  ('a ref, k) Capsule_expert.Data.t ->
  'b t ->
  f:(Msg.t option -> 'a ref -> 'b -> 'c) @ portable ->
  'c
(** [apply_with_capsule c t ~f] applies [f] to the content of [c] and [within]'s
    message and data under the protection of [within]'s mutex. It returns the
    result of this application.

    This is a way to work on the content of a capsule while ensuring mutual
    exclusion with other operations on the shared data structure [within]. *)

val merge :
  ('a : value mod portable) ('b : value mod portable).
  'a t -> within:'b t -> f:('a -> 'b -> 'b) @ portable -> unit
(** [merge t ~within ~f] updates the content of [within] by applying [f] to the
    content of [t] and [within] under the protection of [within]'s mutex. *)

val protect_capsule :
  ('a, k) Capsule.Data.t -> f:('a -> 'b) @ portable -> ('b : immutable_data)
