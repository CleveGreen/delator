val slots :
  unit ->
  (int [@log_value.trace]) ->
  (int [@log_value.info]) ->
  (int [@log_value.error]) ->
  int
