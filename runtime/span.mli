type t
type context

val create : unit -> t
val id : t -> int
val parent : t -> int option
val enter : t -> unit
val exit : t -> int64
val current_context : unit -> context
val current_id_in : context -> int option
val start : context -> int
val enter_started : context -> int -> unit
val finish : int -> int64
val current_id : unit -> int option
val depth : unit -> int
