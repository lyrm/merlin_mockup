@@ portable

open! Await

type 'a t : value mod contended portable
type k
type mutex = k Mutex.t

val create : unit -> 'a t
val data : 'a t -> ('a option ref, k) Capsule.Data.t
val mutex : 'a t -> mutex
val send_and_wait : 'a t -> Msg.t -> unit
val recv_clear : 'a t -> Msg.t
val project : 'a t -> f:('a option -> 'b option) @ portable -> 'b t

val protected_apply :
  ('b : immutable_data).
  'a t -> (Msg.t option -> 'a option -> 'b) @ portable -> 'b

val protected_set : 'a t -> (Msg.t option -> 'a option) @ portable -> unit

val inject_capsule :
  ('c : immutable_data).
  ('a ref, k) Capsule_expert.Data.t ->
  within:'b t @ portable ->
  f:(Msg.t option -> 'a ref -> 'b option -> 'c) @ portable ->
  'c
