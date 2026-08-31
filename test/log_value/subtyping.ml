type metadata = { info : int [@log_value.info] }

let trace_sink () (_value : int [@log_value.trace]) = ()

let run () =
  let[@log_value.info] info_value = 17 in
  let[@log_value.debug] debug_value = 19 in
  let metadata = { info = (info_value [@log_value.info]) } in
  [%log.trace "narrowed metadata"
    ~info:(Delator.Field.int (info_value [@log_value.trace]))
    ~debug:(Delator.Field.int (debug_value [@log_value.trace]))
    ~field:(Delator.Field.int (metadata.info [@log_value.trace]))];
  trace_sink () (info_value [@log_value.trace])

let () =
  run ();
  print_endline "subtyping=ok"
