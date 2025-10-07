@@ portable

type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a option }

val create : unit -> 'a t
val lock : 'a t -> unit
val unlock : 'a t -> unit
val put_ack : 'a t -> 'a -> unit
val take : 'a t -> 'a
val unsafe_get : 'a t -> 'a option
val wait : 'a t -> unit
val signal : 'a t -> unit
val protect : 'a t -> (unit -> 'b) -> 'b
