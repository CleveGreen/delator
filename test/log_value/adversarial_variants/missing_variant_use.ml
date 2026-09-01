type payload = Payload of int * (int [@log_value.trace])

let make () = Payload (1, 2)

let read = function
  | Payload (stable, _) -> stable

