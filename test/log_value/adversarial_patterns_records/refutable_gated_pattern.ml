(* Expected: reject the gated refutable pattern.  Erasing [payload] must not
   turn the zero case into a catch-all case. *)
type packet = { tag : int; payload : int [@log_value.trace] }

let classify packet =
  match packet with
  | { tag = 0; payload = 0 [@log_value.trace] } -> `zero
  | _ -> `other
