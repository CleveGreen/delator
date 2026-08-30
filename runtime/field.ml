type value =
  | String of string
  | Int of int
  | Bool of bool

type t = string * value

let string value = String value
let pp printer value = String (Format.asprintf "%a" printer value)
let int value = Int value
let bool value = Bool value
let exn value = String (Printexc.to_string value)

let render = function
  | String value -> value
  | Int value -> string_of_int value
  | Bool true -> "true"
  | Bool false -> "false"

external write_int : bytes -> int -> int = "delator_write_int" [@@noalloc]

let add_to_buffer ~scratch output = function
  | String value -> Stdlib.Buffer.add_string output value
  | Int value ->
      let length = write_int scratch value in
      if length < 0 then
        invalid_arg "Delator.Field.add_to_buffer: scratch is too small";
      Stdlib.Buffer.add_subbytes output scratch 0 length
  | Bool true -> Stdlib.Buffer.add_string output "true"
  | Bool false -> Stdlib.Buffer.add_string output "false"
