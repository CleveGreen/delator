(* Expected: valid.  The ordinary mutable field is updated without metadata. *)
type state = { mutable count : int }

let bump state value =
  state.count <- value;
  state.count
