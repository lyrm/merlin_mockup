type request = Config of Moconfig.t | Close

val listen : handle:(request -> string) -> unit @@ portable
