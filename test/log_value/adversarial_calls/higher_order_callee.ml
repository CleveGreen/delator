(* Expected: reject before erasure.  The expression callee is unknown, so its
   formal slot cannot authenticate the gated actual. *)
let choose () = fun ~metadata:_metadata () -> ()

let run () =
  let[@log_value.trace] metadata = 7 in
  (choose ()) ~metadata:(metadata [@log_value.trace]) ()
