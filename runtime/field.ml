type value =
  | Null
  | True
  | False
  | String of string
  | Int of int
  | Int64 of int64
  | Float of float
  | Seq of value list * int
  | Map of (string * value) list * int

type t = string * value

module View = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Int64 of int64
    | Float of float
    | String of string
    | Seq of { shown : value list; dropped : int }
    | Map of { shown : (string * value) list; dropped : int }
end

let string value = String value
let int value = Int value
let bool value = if value then True else False
let float value = Float value
let int64 value = Int64 value
let null = Null
let exn value = String (Printexc.to_string value)
let pp printer value = String (Format.asprintf "%a" printer value)
let seq ?(dropped = 0) values = Seq (values, max 0 dropped)
let map ?(dropped = 0) entries = Map (entries, max 0 dropped)

let view = function
  | Null -> View.Null
  | True -> View.Bool true
  | False -> View.Bool false
  | String value -> View.String value
  | Int value -> View.Int value
  | Int64 value -> View.Int64 value
  | Float value -> View.Float value
  | Seq (shown, dropped) -> View.Seq { shown; dropped }
  | Map (shown, dropped) -> View.Map { shown; dropped }

let decimal_scratch_size = 21

external write_int : bytes -> int -> int = "delator_write_int" [@@noalloc]
external write_int64 : bytes -> int64 -> int = "delator_write_int64" [@@noalloc]

let add_decimal ~scratch output length =
  if length < 0 then
    invalid_arg "Delator.Field.add_to_buffer: scratch is too small";
  Stdlib.Buffer.add_subbytes output scratch 0 length

let lacks_fraction_marker text =
  not
    (String.exists
       (fun character -> character = '.' || character = 'e' || character = 'E')
       text)

let float_to_string value =
  match classify_float value with
  | FP_nan -> "nan"
  | FP_infinite -> if value > 0. then "inf" else "-inf"
  | FP_zero | FP_normal | FP_subnormal ->
      let shortest =
        let fifteen_digits = Printf.sprintf "%.15g" value in
        if float_of_string fifteen_digits = value then fifteen_digits
        else Printf.sprintf "%.17g" value
      in
      if lacks_fraction_marker shortest then shortest ^ "." else shortest

let is_nonempty = function [] -> false | _ :: _ -> true

let rec add_to_buffer ~scratch output value =
  match value with
  | Null -> Stdlib.Buffer.add_string output "null"
  | True -> Stdlib.Buffer.add_string output "true"
  | False -> Stdlib.Buffer.add_string output "false"
  | String value -> Stdlib.Buffer.add_string output value
  | Int value -> add_decimal ~scratch output (write_int scratch value)
  | Int64 value -> add_decimal ~scratch output (write_int64 scratch value)
  | Float value -> Stdlib.Buffer.add_string output (float_to_string value)
  | Seq (shown, dropped) ->
      Stdlib.Buffer.add_char output '[';
      add_elements ~scratch output shown;
      add_dropped ~scratch output ~after_elements:(is_nonempty shown) dropped;
      Stdlib.Buffer.add_char output ']'
  | Map (shown, dropped) ->
      Stdlib.Buffer.add_char output '{';
      add_entries ~scratch output shown;
      add_dropped ~scratch output ~after_elements:(is_nonempty shown) dropped;
      Stdlib.Buffer.add_char output '}'

and add_elements ~scratch output = function
  | [] -> ()
  | [ value ] -> add_to_buffer ~scratch output value
  | value :: rest ->
      add_to_buffer ~scratch output value;
      Stdlib.Buffer.add_char output ',';
      add_elements ~scratch output rest

and add_entries ~scratch output = function
  | [] -> ()
  | [ entry ] -> add_entry ~scratch output entry
  | entry :: rest ->
      add_entry ~scratch output entry;
      Stdlib.Buffer.add_char output ',';
      add_entries ~scratch output rest

and add_entry ~scratch output (name, value) =
  Stdlib.Buffer.add_string output name;
  Stdlib.Buffer.add_char output '=';
  add_to_buffer ~scratch output value

and add_dropped ~scratch output ~after_elements dropped =
  if dropped > 0 then begin
    if after_elements then Stdlib.Buffer.add_char output ',';
    Stdlib.Buffer.add_char output '+';
    add_decimal ~scratch output (write_int scratch dropped)
  end

let render value =
  match value with
  | String value -> value
  | Int value -> string_of_int value
  | True -> "true"
  | False -> "false"
  | Null -> "null"
  | Int64 value -> Int64.to_string value
  | Float value -> float_to_string value
  | Seq _ | Map _ ->
      let output = Stdlib.Buffer.create 64 in
      add_to_buffer ~scratch:(Bytes.create decimal_scratch_size) output value;
      Stdlib.Buffer.contents output

let fold ~string ~int ~bool value =
  match value with
  | Int value -> int value
  | True -> bool true
  | False -> bool false
  | Null | String _ | Int64 _ | Float _ | Seq _ | Map _ -> string (render value)
