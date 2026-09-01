(* Expected: reject.  Removing the only formal changes the function shape and
   can move evaluation of the body across the call boundary. *)
let make () =
  let generated =
    fun (value [@log_value.trace]) ->
      ignore (Sys.opaque_identity (value [@log_value.trace]));
      0
  in
  generated
