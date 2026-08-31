let run input =
  let[@log_value.trace] trace_value = input + 1 in
  [%log.debug "bad"
    ~trace_value:
      (Delator.Field.int (trace_value [@log_value.debug]))]
