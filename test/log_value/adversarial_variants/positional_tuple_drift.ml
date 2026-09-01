type payload = int * (int [@log_value.trace]) * int

let make () =
  (( 1,
     (2 [@log_value.trace]),
     (3 [@log_value.trace]) ) : payload)
