(* Expected: valid.  An ordinary formal preserves the function boundary; the
   gated value is only additional metadata. *)
let make () =
  fun ordinary (_value [@log_value.trace]) -> ordinary
