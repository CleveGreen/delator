let run input =
  let[@log_value.debug] debug_value = input + 1 in
  [%log.info "bad"
    ~debug_value:
      (Delator.Field.int (debug_value [@log_value.debug]))]
