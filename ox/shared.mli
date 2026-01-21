@@ portable

open! Await

type lock
type 'a t : value mod contended portable

val create : unit -> 'a t
val mutex : 'a t -> lock Mutex.t
val data : 'a t -> ('a option ref, lock) Capsule.Data.t
val cond : 'a t -> lock Mutex.Condition.t
val send_and_wait : 'a t -> Msg.t -> unit
val recv_clear : 'a t -> Msg.t
val project : 'a t -> f:('a option -> 'b option) @ portable -> 'b t

val protected_apply :
  ('b : immutable_data).
  'a t -> (Msg.t option -> 'a option -> 'b) @ portable -> 'b

val protected_set : 'a t -> (Msg.t option -> 'a option) @ portable -> unit
