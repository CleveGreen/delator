type metadata = int * (int [@log_value.trace])

let ordinary_pair = (1, 2)
let () = Printf.printf "ordinary=%d\n" (fst ordinary_pair)
