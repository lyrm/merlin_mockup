@@ portable

type completion = All | Part of int
type config = { source : string; completion : completion }

type msg =
  | Msg of [ `Cancel | `Closing | `Exn of exn ] @@ many portable
  | Config of config
  | Partial_is_available

type t : value mod contended portable

val create : unit -> t
val send_and_wait : t -> msg -> unit
val recv_clear : t -> msg

val protected_apply :
  ('a : immutable_data).
  t -> (msg option -> Mopipeline.t option -> 'a) @ portable -> 'a

(* type ('a : immutable_data) on_pipeline = msg option -> Mopipeline.t option -> 'a

val protected_apply : 'k t -> 'a on_pipeline @ portable -> 'a
 *)
