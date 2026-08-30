type t = Trace | Debug | Info | Warn | Error

val compare : t -> t -> int
val to_int : t -> int
val of_string : string -> (t, string) result
val to_string : t -> string
val is_color_enabled : unit -> bool
val set_color_enabled : bool -> unit
val configure_color_from_env : unit -> unit
val add_to_buffer : Stdlib.Buffer.t -> t -> unit
val add_span_marker : Stdlib.Buffer.t -> string -> unit
