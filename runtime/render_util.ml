let add_indent output depth =
  for _ = 1 to depth * 2 do
    Stdlib.Buffer.add_char output ' '
  done

let rec add_fields ~scratch output = function
  | [] -> ()
  | (name, value) :: rest ->
      Stdlib.Buffer.add_char output ' ';
      Stdlib.Buffer.add_string output name;
      Stdlib.Buffer.add_char output '=';
      Field.add_to_buffer ~scratch output value;
      add_fields ~scratch output rest

external write_int : bytes -> int -> int = "delator_write_int" [@@noalloc]
external write_int64 : bytes -> int64 -> int = "delator_write_int64" [@@noalloc]

let add_int ~scratch output value =
  let length = write_int scratch value in
  if length < 0 then invalid_arg "Delator integer scratch is too small";
  Stdlib.Buffer.add_subbytes output scratch 0 length

let add_int64 ~scratch output value =
  let length = write_int64 scratch value in
  if length < 0 then invalid_arg "Delator int64 scratch is too small";
  Stdlib.Buffer.add_subbytes output scratch 0 length
