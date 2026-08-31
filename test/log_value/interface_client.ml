let () =
  let value =
    Log_value_interface.make
      ((failwith "trace actual evaluated") [@log_value.trace])
      ((failwith "debug actual evaluated") [@log_value.debug])
      7
  in
  let collapsed =
    Log_value_interface.collapse
      ()
      ((failwith "trace collapsed actual evaluated") [@log_value.trace])
      ((failwith "debug collapsed actual evaluated") [@log_value.debug])
  in
  let Log_value_interface.Collapsed
        ((_ [@log_value.trace]), (_ [@log_value.debug])) = collapsed
  in
  [%log.trace "helper"
    ~value:(Delator.Field.int (Log_value_interface.trace_helper [@log_value.trace]))];
  Printf.printf "interface=%d\n" value.run
