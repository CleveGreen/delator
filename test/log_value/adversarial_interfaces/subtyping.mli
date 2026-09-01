type t = {
  stable : int;
  info : int [@log_value.info];
}

val make : stable:int -> info:(int [@log_value.info]) -> t
val consume : unit -> (int [@log_value.info]) -> unit
val info_value : int [@@log_value.info]
val trace_value : int [@@log_value.trace]
