type unary = C of (int [@log_value.trace])
type binary = C of int * (int [@log_value.trace])

let unary : unary = C (1 [@log_value.trace])
let binary : binary = C (1, (2 [@log_value.trace]))
