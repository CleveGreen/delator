let consume () (_value : int [@log_value.trace]) = ()

let run value = consume () (value [@log_value.debug])
