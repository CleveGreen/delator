val is_enabled : level:Level.t -> target:string -> bool
val set_default : Level.t -> unit
val default : unit -> Level.t
val configure_from_env : unit -> unit
