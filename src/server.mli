type request = Config of Moconfig.t | Close
type t = Request of request | Bad_request

val listen : handle:(request -> string) -> unit
