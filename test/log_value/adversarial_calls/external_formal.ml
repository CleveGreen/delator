external consume : (int [@log_value.trace]) -> unit -> unit
  = "delator_test_consume"

let run () =
  let[@log_value.trace] metadata = 7 in
  consume (metadata [@log_value.trace]) ()
