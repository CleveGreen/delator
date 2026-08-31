let run input =
  let[@log_value.trace] trace_value = input + 1 in
  [%log.trace "bad" ~trace_value:(Delator.Field.int trace_value)]
