module Metadata : sig
  val consume : (int [@log_value.trace]) -> unit -> unit
end
