module Level = Level
module Field = Field
module Renderer = Renderer
module Color : sig
  val is_enabled : unit -> bool
  val set_enabled : bool -> unit
end
module Clock : sig
  val now_ns : unit -> int64
  val set : (unit -> int64) -> unit
  val disable : unit -> unit
  val use_monotonic : unit -> unit
  val use_tsc : unit -> unit
end
module Runtime = Runtime

type level = Level.t = Trace | Debug | Info | Warn | Error

val init : unit -> unit
val set_default_level : level -> unit
val in_span :
  level:level -> target:string -> name:string ->
  ?fields:(unit -> Field.t list) -> ?log_exn:bool -> (unit -> 'a) -> 'a
