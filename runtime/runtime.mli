type span = Span.t

val initialize : unit -> unit
val is_enabled : level:Level.t -> target:string -> bool
val event : target:string -> level:Level.t -> msg:string -> fields:Field.t list -> unit
val new_span : target:string -> level:Level.t -> name:string -> fields:Field.t list -> span
val enter : span -> unit
val exit : span -> unit
