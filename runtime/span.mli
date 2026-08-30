type t

val create : name:string -> target:string -> level:Level.t -> Field.t list -> t
val id : t -> int
val parent : t -> int option
val name : t -> string
val target : t -> string
val level : t -> Level.t
val fields : t -> Field.t list
val enter : t -> unit
val exit : t -> int64
val current_id : unit -> int option
val depth : unit -> int
