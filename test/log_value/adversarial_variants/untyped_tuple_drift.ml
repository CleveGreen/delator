let value = ((1 [@log_value.trace]), 2)
let read (x, (_ [@log_value.trace])) = x
let () = Printf.printf "%d\n" (read value)
