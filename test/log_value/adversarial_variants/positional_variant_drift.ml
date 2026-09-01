type payload =
  | Payload of int * (int [@log_value.trace]) * int

let make () =
  Payload
    ( 1,
      (2 [@log_value.trace]),
      (3 [@log_value.trace]) )

