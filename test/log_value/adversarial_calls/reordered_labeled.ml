(* Expected: reject before erasure.  Matching is by label, not by the order
   in which labeled actuals happen to occur. *)
let consume ~trace:(_trace : int [@log_value.trace])
    ~debug:(_debug : int [@log_value.debug]) () = ()

let run () =
  let[@log_value.trace] trace_value = 7 in
  let[@log_value.debug] debug_value = 11 in
  consume
    ~debug:(trace_value [@log_value.trace])
    ~trace:(debug_value [@log_value.debug]) ()

let () = run ()
