let run input =
  let[@log_value.trace] rec loop value =
    if value = 0 then input else loop (value - 1)
  in
  loop 1
