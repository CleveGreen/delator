val f :
  int ->
  trace:(int [@log_value.trace]) ->
  string ->
  debug:(int [@log_value.debug]) ->
  unit ->
  int
