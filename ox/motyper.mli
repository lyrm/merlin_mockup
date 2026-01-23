type parsedtree = (string * int) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

val run : Moconfig.t -> parsedtree Shared.t -> result Shared.t
