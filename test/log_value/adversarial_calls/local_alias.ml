(* Expected: reject before erasure.  An alias does not authenticate a gated
   actual when the aliased formal is ordinary. *)
let consume ~metadata:_metadata () = ()
let alias = consume

let run () =
  let[@log_value.trace] metadata = 7 in
  alias ~metadata:(metadata [@log_value.trace]) ()
