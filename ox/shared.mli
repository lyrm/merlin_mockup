@@ portable

open! Await

type ('a, 'k) t : value mod contended portable = {
  msg : (Msg.t option ref, 'k) Capsule.Data.t;
  data : ('a option ref, 'k) Capsule.Data.t;
  mutex : 'k Mutex.t;
  cond : 'k Mutex.Condition.t;
}
(* Why we can't abstract [t] *)

type 'a packed : value mod contended portable = P : ('a, 'k) t -> 'a packed
[@@unboxed]

val create : unit -> 'a packed
val send_and_wait : 'a packed -> Msg.t -> unit
val recv_clear : 'a packed -> Msg.t
val project : ('a, 'k) t -> f:('a option -> 'b option) @ portable -> ('b, 'k) t

val protected_apply :
  ('b : immutable_data).
  'a packed -> (Msg.t option -> 'a option -> 'b) @ portable -> 'b

val protected_set : 'a packed -> (Msg.t option -> 'a option) @ portable -> unit
