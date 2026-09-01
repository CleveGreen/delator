(* Expected: valid.  The gated component is bound irrefutably; the ordinary
   tag remains responsible for control flow. *)
type packet = { tag : int; payload : int [@log_value.trace] }

let classify packet =
  match packet with
  | { tag; payload = (_ [@log_value.trace]) } ->
      if tag = 0 then `zero else `other
