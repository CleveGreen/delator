module Level : sig @@ portable
  include module type of Level
end
module Field : sig @@ portable
  include module type of Field
end
module Renderer : sig @@ portable
  include module type of Renderer
end
module Color : sig @@ portable
  val is_enabled : unit -> bool
  val set_enabled : bool -> unit
end
module Clock : sig @@ portable
  val now_ns : unit -> int64
  val set : (unit -> int64) -> unit
  val disable : unit -> unit
  val use_monotonic : unit -> unit
  val use_tsc : unit -> unit
end
module Runtime : sig @@ portable
  include module type of Runtime
end

type level = Level.t = Trace | Debug | Info | Warn | Error

val init : unit -> unit @@ portable
val set_default_level : level -> unit @@ portable
val in_span :
  level:level -> target:string -> name:string ->
  ?fields:(unit -> Field.t list) @ local -> ?log_exn:bool ->
  (unit -> 'a) @ local -> 'a @@ portable
