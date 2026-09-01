(* Expected: valid.  The returned closure retains an ordinary parameter and
   uses the gated capture at a compatible annotated site. *)
let make () =
  let[@log_value.trace] captured = Sys.opaque_identity 7 in
  let[@log_value.trace] _trace_sink = captured [@log_value.trace] in
  fun ordinary -> ordinary
