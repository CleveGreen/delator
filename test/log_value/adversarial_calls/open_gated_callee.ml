module Metadata = struct
  let consume (_metadata [@log_value.trace]) () = ()
end

open Metadata

let run () =
  let[@log_value.trace] metadata = 1 in
  consume (metadata [@log_value.trace]) ()
