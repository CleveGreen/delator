let pair = (0, 0)

let run () =
  let pair = ((1 [@log_value.trace]), 2)
  and alias = pair in
  ignore alias;
  let (_ [@log_value.trace]), ordinary = pair in
  ordinary
