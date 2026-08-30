type value
type t = string * value

val string : string -> value
val pp : (Format.formatter -> 'a -> unit) -> 'a -> value
val int : int -> value
val bool : bool -> value
val exn : exn -> value
val render : value -> string
