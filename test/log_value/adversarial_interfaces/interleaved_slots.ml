let f
    ~trace:(_trace : int [@log_value.trace])
    ordinary
    ~debug:(_debug : int [@log_value.debug])
    text
    () =
  ignore text;
  ordinary
