(* Expected: reject before erasure.  Authentication must cover supplied
   arguments even when the application is partial; the Trace actual is being
   supplied to the Debug formal. *)
let consume ~trace:(_trace : int [@log_value.trace])
    ~debug:(_debug : int [@log_value.debug]) () = ()

let run () =
  let[@log_value.trace] metadata = 7 in
  consume ~debug:(metadata [@log_value.trace])
