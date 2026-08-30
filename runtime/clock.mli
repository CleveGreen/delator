type sample

val zero_sample : sample
val is_enabled : unit -> bool
val now_ns : unit -> int64
val sample : unit -> sample
val elapsed_ns : sample -> int64
val set : (unit -> int64) -> unit
val disable : unit -> unit
val use_monotonic : unit -> unit
val use_tsc : unit -> unit
val configure_from_env : unit -> unit
