let count source_members =
  let[@log_value.debug] count =
    List.fold_left
      ((fun count member -> count + List.length member) [@log_value.debug])
      0 source_members
  in
  let[@log_value.debug] _observed = count [@log_value.debug] in
  ()
