type value
type t = string * value

val string : string -> value
val int : int -> value
val bool : bool -> value
val float : float -> value
val int64 : int64 -> value
val null : value
val exn : exn -> value
val pp : (Format.formatter -> 'a -> unit) -> 'a -> value
val seq : ?dropped:int -> value list -> value
val map : ?dropped:int -> (string * value) list -> value

module View : sig
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Int64 of int64
    | Float of float
    | String of string
    | Seq of { shown : value list; dropped : int }
    | Map of { shown : (string * value) list; dropped : int }
end

val view : value -> View.t
val render : value -> string
val add_to_buffer : scratch:bytes -> Stdlib.Buffer.t -> value -> unit

val fold :
  string:(string -> 'a) -> int:(int -> 'a) -> bool:(bool -> 'a) -> value -> 'a
  [@@ocaml.deprecated
    "Use Delator.Field.view. [fold] renders every value outside int and bool \
     through its ~string case."]
