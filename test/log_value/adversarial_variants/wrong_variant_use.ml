type payload = Payload of int * (int [@log_value.trace])

let make () = Payload (1, (2 [@log_value.debug]))

let read (Payload (stable, (_ [@log_value.debug]))) = stable

