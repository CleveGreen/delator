type payload =
  | Payload of (int [@log_value.trace])

let make () = Payload (7 [@log_value.trace])

let read (Payload (_ [@log_value.trace])) = 7

let () = Printf.printf "unary=%d\n" (read (make ()))

