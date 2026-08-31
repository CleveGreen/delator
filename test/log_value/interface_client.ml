let () =
  let value =
    Log_value_interface.make
      ((failwith "trace actual evaluated") [@log_value.trace])
      ((failwith "debug actual evaluated") [@log_value.debug])
      7
  in
  [%log.trace "helper"
    ~value:(Delator.Field.int (Log_value_interface.trace_helper [@log_value.trace]))];
  Printf.printf "interface=%d\n" value.run
