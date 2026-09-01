let trace_sink () (_value : int [@log_value.trace]) = ()

let run (value : Subtyping.t) =
  let[@log_value.info] info = 19 in
  Subtyping.consume () (info [@log_value.info]);
  trace_sink () (Subtyping.info_value [@log_value.trace]);
  trace_sink () (value.Subtyping.info [@log_value.trace])
