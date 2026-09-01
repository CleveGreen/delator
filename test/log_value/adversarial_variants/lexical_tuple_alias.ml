let run () =
  let value = (1, (2 [@log_value.trace]), 3) in
  let alias = value in
  let (stable, (_ [@log_value.trace]), last) = alias in
  stable + last
