type payload = int * (int [@log_value.trace]) * int

let make () = ((1, (2 [@log_value.trace]), 3) : payload)

let read (((stable [@log_value.trace]), _, last) : payload) =
  (stable [@log_value.trace]) + last
