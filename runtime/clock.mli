type sample

val now_ns : unit -> int64
val sample : unit -> sample
val elapsed_ns : sample -> int64
val set : (unit -> int64) -> unit
val use_monotonic : unit -> unit
val use_tsc : unit -> unit
val configure_from_env : unit -> unit
