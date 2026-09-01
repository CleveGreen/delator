type origin = Exact_alias

type 'fact source =
  | Existing_fact of 'fact
  | Exact_alias of 'fact
  | Exact_let of 'fact

let fact = function
  | Existing_fact fact | Exact_alias fact | Exact_let fact -> fact
