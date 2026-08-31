type collapsed =
  | Collapsed of (int [@log_value.trace]) * (int [@log_value.debug])

let effects = ref 0

let _effect value =
  incr effects;
  value

let run () =
  let value =
    Collapsed
      ( (_effect 11 [@log_value.trace]),
        (_effect 13 [@log_value.debug]) )
  in
  let tuple =
    ( (_effect 17 [@log_value.trace]),
      (_effect 19 [@log_value.debug]) )
  in
  let Collapsed ((_ [@log_value.trace]), (_ [@log_value.debug])) = value in
  let ((_ [@log_value.trace]), (_ [@log_value.debug])) = tuple in
  18

let () =
  let result = run () in
  Printf.printf "complete=%d effects=%d\n" result !effects
