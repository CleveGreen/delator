let read = function
  | Ordinary_constructor.Parametric_symbolic_application
      (_application [@log_value.debug]) ->
      let[@log_value.debug] _value =
        _application [@log_value.debug]
      in
      ()
