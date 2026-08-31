type t = {
  run : int;
  trace : int [@log_value.trace];
  debug : int [@log_value.debug];
}

let make
    (trace : int [@log_value.trace])
    (debug : int [@log_value.debug])
    run =
  {
    run;
    trace = (trace [@log_value.trace]);
    debug = (debug [@log_value.debug]);
  }

let[@log_value.trace] trace_helper = 42
