type t = {
  stable : int;
  trace : int [@log_value.trace];
  debug : int [@log_value.debug];
}

val make :
  stable:int ->
  trace:(int [@log_value.trace]) ->
  debug:(int [@log_value.debug]) ->
  t
