module Level = Level
module Field = Field
module Renderer = Renderer
module Clock = Clock
module Runtime = Runtime

type level = Level.t = Trace | Debug | Info | Warn | Error

val init : unit -> unit
val set_default_level : level -> unit
val in_span :
  level:level -> target:string -> name:string ->
  ?fields:(unit -> Field.t list) -> ?log_exn:bool -> (unit -> 'a) -> 'a
