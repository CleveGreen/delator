type t = {
  run : int;
  trace : int [@log_value.trace];
  debug : int [@log_value.debug];
}

val make :
  (int [@log_value.trace]) ->
  (int [@log_value.debug]) ->
  int ->
  t

val trace_helper : int [@@log_value.trace]
