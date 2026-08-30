module type S = Renderer_intf.S

val tree : (module S)
val flat : (module S)
val set_current : (module S) -> unit
val configure_from_env : unit -> unit
val on_new_span :
  id:int -> parent:int option -> name:string -> target:string ->
  level:Level.t -> fields:Field.t list -> unit
val on_exit : id:int -> duration_ns:int64 -> unit
val on_event :
  span:int option -> target:string -> level:Level.t -> msg:string ->
  fields:Field.t list -> unit
