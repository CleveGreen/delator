let f ordinary (_metadata : int [@log_value.trace]) =
  print_endline "body";
  ordinary

let _partial = f 1
let () = print_endline "done"
