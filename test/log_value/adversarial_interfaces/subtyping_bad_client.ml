let debug_sink () (_value : int [@log_value.debug]) = ()

let run () =
  debug_sink () (Subtyping.trace_value [@log_value.debug])
