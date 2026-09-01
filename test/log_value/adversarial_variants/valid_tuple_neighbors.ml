type payload = int * (int [@log_value.trace]) * (int [@log_value.debug])

let effects = ref 0

let mark name value =
  incr effects;
  Printf.printf "%s\n" name;
  value

let () =
  let value : payload =
    ( mark "stable" 1,
      (mark "trace" 2 [@log_value.trace]),
      (mark "debug" 3 [@log_value.debug]) )
  in
  let (stable, (_ [@log_value.trace]), (_ [@log_value.debug])) = value in
  Printf.printf "stable=%d effects=%d\n" stable !effects
