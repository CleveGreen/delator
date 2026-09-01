type t = {
  stable : int;
  trace : int [@log_value.trace];
  debug : int [@log_value.debug];
}

let make
    ~stable
    ~(trace : int [@log_value.trace])
    ~(debug : int [@log_value.debug]) =
  { stable; trace = (trace [@log_value.trace]); debug = (debug [@log_value.debug]) }
