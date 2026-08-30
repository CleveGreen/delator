let add_indent output depth =
  for _ = 1 to depth * 2 do
    Stdlib.Buffer.add_char output ' '
  done

let rec add_fields output = function
  | [] -> ()
  | (name, value) :: rest ->
      Stdlib.Buffer.add_char output ' ';
      Stdlib.Buffer.add_string output name;
      Stdlib.Buffer.add_char output '=';
      Stdlib.Buffer.add_string output (Field.render value);
      add_fields output rest

external write_int : bytes -> int -> int = "delator_write_int" [@@noalloc]
external write_int64 : bytes -> int64 -> int = "delator_write_int64" [@@noalloc]

let add_int ~scratch output value =
  let length = write_int scratch value in
  Stdlib.Buffer.add_subbytes output scratch 0 length

let add_int64 ~scratch output value =
  let length = write_int64 scratch value in
  Stdlib.Buffer.add_subbytes output scratch 0 length
