type t = Trace | Debug | Info | Warn | Error

val compare : t -> t -> int
val to_int : t -> int
val of_string : string -> (t, string) result
val to_string : t -> string
