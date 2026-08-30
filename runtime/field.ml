type value = string
type t = string * value

let string value = value
let pp printer value = Format.asprintf "%a" printer value
let int = string_of_int
let bool = string_of_bool
let exn = Printexc.to_string
let render value = value
