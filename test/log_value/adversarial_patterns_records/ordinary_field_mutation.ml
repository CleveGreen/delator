(* Expected: reject.  An annotation on an ordinary mutable field cannot erase
   the update without changing the observable state transition. *)
type state = { mutable count : int }

let bump state value =
  state.count <- value [@log_value.trace];
  state.count
