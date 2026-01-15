type t =
  | Msg of [ `Cancel | `Closing | `Exn of exn ] @@ many portable
  | Config of Moconfig.t
  | Partial_is_available
