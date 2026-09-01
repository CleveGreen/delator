type payload = int * (int [@log_value.trace])

let value : payload = (1, (2 [@log_value.trace]))
let escape () = Sys.opaque_identity value
