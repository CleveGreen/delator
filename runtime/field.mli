type value
type t = string * value

val string : string -> value
val pp : (Format.formatter -> 'a -> unit) -> 'a -> value
val int : int -> value
val bool : bool -> value
val exn : exn -> value
val fold :
  string:(string -> 'a) -> int:(int -> 'a) -> bool:(bool -> 'a) -> value -> 'a
val render : value -> string
val add_to_buffer : scratch:bytes -> Stdlib.Buffer.t -> value -> unit
