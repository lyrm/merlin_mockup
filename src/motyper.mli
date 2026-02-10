@@ portable

type parsedtree = (string * Moparser.expr) list
type typedtree = (string * int) list ref
type result = { config : Moconfig.t; typedtree : typedtree }

exception Cancel_or_closing
exception Exception_after_partial of exn

type partial = Type_implem | Run of result Hermes.t
type _ eff = Partial : partial -> unit eff

module Eff : Handled_effect.S with type ('a, 'e) ops := 'a eff

val run :
  Moconfig.t ->
  _ Hermes.t ->
  handler:local_ Eff.t Handled_effect.Handler.t ->
  parsedtree ->
  result Hermes.t
