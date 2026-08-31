exception Raised

let _raised_constructor = Raised

let events = Buffer.create 16

let mark event value =
  Buffer.add_char events event;
  value

let take_trace () (_value : int [@log_value.trace]) = ()
let take_debug () (_value : int [@log_value.debug]) = ()
let take_info () (_value : int [@log_value.info]) = ()

let evaluation_order () =
  let[@log_value.trace] trace = mark 't' 1 in
  let ordinary = mark 'o' 2 in
  let[@log_value.debug] debug = mark 'd' 3 in
  let[@log_value.info] info = mark 'i' 4 in
  take_trace () (trace [@log_value.trace]);
  take_debug () (debug [@log_value.debug]);
  take_info () (info [@log_value.info]);
  ordinary

let exceptional_evaluation () =
  try
    let[@log_value.debug] _raised = raise Raised in
    "returned"
  with Raised -> "raised"

let closure_capture () =
  let[@log_value.debug] captured = mark 'c' 5 in
  let call () = take_debug () (captured [@log_value.debug]) in
  call ();
  call ()

let lexical_shadowing () =
  let[@log_value.trace] value = mark 's' 6 in
  let use_outer () = take_trace () (value [@log_value.trace]) in
  let value = 7 in
  let take_trace ordinary = ordinary in
  use_outer ();
  ignore (take_trace value);
  value

let () =
  let ordinary = evaluation_order () in
  let exception_result = exceptional_evaluation () in
  closure_capture ();
  let shadow = lexical_shadowing () in
  Printf.printf "events=%s ordinary=%d exception=%s shadow=%d\n"
    (Buffer.contents events) ordinary exception_result shadow
