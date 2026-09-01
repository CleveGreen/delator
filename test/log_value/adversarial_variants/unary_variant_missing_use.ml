type payload =
  | Payload of (int [@log_value.trace])

let make () = Payload (7 [@log_value.trace])

let read (Payload _) = 7

