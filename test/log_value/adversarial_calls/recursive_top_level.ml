let rec consume (metadata [@log_value.trace]) remaining =
  if remaining = 0 then ()
  else consume (metadata [@log_value.trace]) (remaining - 1)

let run () =
  let[@log_value.trace] metadata = 7 in
  consume (metadata [@log_value.trace]) 2
