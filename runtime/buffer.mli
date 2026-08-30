type line

val line_buffer : unit -> line
val output : line -> Stdlib.Buffer.t
val decimal_scratch : line -> bytes
val finish_line : line -> unit
val flush : unit -> unit
val configure_from_env : unit -> unit
val install_at_exit : unit -> unit
