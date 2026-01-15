@@ portable

type t : value mod contended portable

val create : unit -> t
val send_and_wait : t -> Msg.t -> unit
val recv_clear : t -> Msg.t

val protected_apply :
  ('a : immutable_data).
  t -> (Msg.t option -> Mopipeline.t option -> 'a) @ portable -> 'a

val protected_update :
  t ->
  (Msg.t option -> Mopipeline.t option -> Mopipeline.t option) @ portable ->
  unit
