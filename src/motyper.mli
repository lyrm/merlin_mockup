type parsedtree = (string * Moparser.expr) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

type msg =
  | Msg of [ `Cancel | `Close | `Exn of exn ]
  | Config of Moconfig.t
  | Partial of result

type partial = Type_implem of typedtree | Run of result
type _ Effect.t += Partial : partial -> unit Effect.t

val run : Moconfig.t -> msg Hermes.t -> parsedtree -> result
