(* Expected: reject the ambiguous shared label.  Both records retain an
   ordinary field, so the fully-gated-record guard is not the reason this is
   unsafe.  The trace annotation on [shared] targets the ordinary field in
   [trace_record], but the duplicate label has a different contract in the
   other record. *)
type trace_record = { shared : int; trace_unique : int }
type ordinary_record = { shared : int [@log_value.trace]; ordinary_unique : int }

let trace_record value : trace_record =
  { shared = value [@log_value.trace]; trace_unique = 0 }

let update_trace value record : trace_record =
  { record with shared = value [@log_value.trace] }
