type t = {
  stable : int;
  trace : int [@log_value.debug];
  debug : int [@log_value.trace];
}

let make
    ~stable
    ~(trace : int [@log_value.debug])
    ~(debug : int [@log_value.trace]) =
  { stable; trace = (trace [@log_value.debug]); debug = (debug [@log_value.trace]) }
