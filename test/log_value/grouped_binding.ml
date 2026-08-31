let run input =
  let[@log_value.trace] left = input
  and right = input + 1 in
  left + right
