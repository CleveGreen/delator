(* Expected: valid.  Distinct labels avoid an erasure contract collision. *)
type trace_record = {
  trace_shared : int [@log_value.trace];
  trace_unique : int;
}

type ordinary_record = { ordinary_shared : int; ordinary_unique : int }

let make_trace value : trace_record =
  { trace_shared = value [@log_value.trace]; trace_unique = 0 }

let make_ordinary value : ordinary_record =
  { ordinary_shared = value; ordinary_unique = 0 }
