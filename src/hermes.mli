type 'a t

val create : unit -> 'a t
val send_and_wait : 'a t -> 'a -> unit
val recv_clear : 'a t -> 'a
val unsafe_get : 'a t -> 'a option
val wait : 'a t -> unit
val signal : 'a t -> unit
val protect : 'a t -> (unit -> 'b) -> 'b
