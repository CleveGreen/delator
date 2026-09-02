[@@@alert "-deprecated"]

module Field = Delator.Field

let decimal_scratch () = Bytes.create 21

let buffered value =
  let output = Stdlib.Buffer.create 32 in
  Field.add_to_buffer ~scratch:(decimal_scratch ()) output value;
  Stdlib.Buffer.contents output

let check_text value expected =
  assert (Field.render value = expected);
  assert (buffered value = expected)

let expect_invalid operation =
  match operation () with
  | () -> failwith "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let folded value =
  Field.fold value
    ~string:(fun value -> "string:" ^ value)
    ~int:(fun value -> "int:" ^ string_of_int value)
    ~bool:(fun value -> "bool:" ^ string_of_bool value)

let () =
  assert (Field.view Field.null = Field.View.Null);
  assert (Field.view (Field.bool true) = Field.View.Bool true);
  assert (Field.view (Field.bool false) = Field.View.Bool false);
  assert (Field.view (Field.int 3) = Field.View.Int 3);
  assert (Field.view (Field.int64 9L) = Field.View.Int64 9L);
  assert (Field.view (Field.float 1.5) = Field.View.Float 1.5);
  assert (Field.view (Field.string "x") = Field.View.String "x");
  assert (Field.view (Field.exn (Failure "boom")) = Field.View.String "Failure(\"boom\")");
  assert (Field.view (Field.pp Format.pp_print_int 42) = Field.View.String "42");
  assert (
    Field.view (Field.seq ~dropped:2 [ Field.int 1 ])
    = Field.View.Seq { shown = [ Field.int 1 ]; dropped = 2 });
  assert (
    Field.view (Field.map [ ("a", Field.int 1) ])
    = Field.View.Map { shown = [ ("a", Field.int 1) ]; dropped = 0 })

let () =
  assert (Obj.is_int (Obj.repr Field.null));
  assert (Obj.is_int (Obj.repr (Field.bool true)));
  assert (Obj.is_int (Obj.repr (Field.bool false)))

let () =
  check_text Field.null "null";
  check_text (Field.bool true) "true";
  check_text (Field.bool false) "false";
  check_text (Field.string "plain") "plain";
  check_text (Field.int min_int) (string_of_int min_int);
  check_text (Field.int64 Int64.min_int) (Int64.to_string Int64.min_int);
  check_text (Field.int64 Int64.max_int) (Int64.to_string Int64.max_int)

let () =
  check_text (Field.float 1.0) "1.";
  check_text (Field.float (-0.)) "-0.";
  check_text (Field.float 1.5) "1.5";
  check_text (Field.float 0.1) "0.1";
  check_text (Field.float (0.1 +. 0.2)) "0.30000000000000004";
  check_text (Field.float 1e300) "1e+300";
  check_text (Field.float nan) "nan";
  check_text (Field.float infinity) "inf";
  check_text (Field.float neg_infinity) "-inf";
  assert (float_of_string (Field.render (Field.float 0.1)) = 0.1);
  assert (float_of_string (Field.render (Field.float (0.1 +. 0.2))) = 0.1 +. 0.2)

let () =
  check_text (Field.seq []) "[]";
  check_text (Field.seq [ Field.int 1; Field.int 2 ]) "[1,2]";
  check_text (Field.seq ~dropped:97 [ Field.int 1; Field.int 2; Field.int 3 ]) "[1,2,3,+97]";
  check_text (Field.seq ~dropped:5 []) "[+5]";
  check_text (Field.seq ~dropped:(-1) [ Field.int 1 ]) "[1]";
  assert (
    Field.view (Field.map ~dropped:(-1) [])
    = Field.View.Map { shown = []; dropped = 0 });
  check_text (Field.map []) "{}";
  check_text (Field.map [ ("a", Field.int 1); ("b", Field.string "x") ]) "{a=1,b=x}";
  check_text (Field.map ~dropped:2 [ ("a", Field.null) ]) "{a=null,+2}";
  check_text
    (Field.seq [ Field.map [ ("k", Field.seq [ Field.bool true ]) ] ])
    "[{k=[true]}]"

let () =
  assert (folded (Field.int 3) = "int:3");
  assert (folded (Field.bool false) = "bool:false");
  assert (folded (Field.string "x") = "string:x");
  assert (folded Field.null = "string:null");
  assert (folded (Field.float 1.5) = "string:1.5");
  assert (folded (Field.int64 9L) = "string:9");
  assert (folded (Field.seq [ Field.int 1 ]) = "string:[1]");
  assert (folded (Field.map [ ("a", Field.int 1) ]) = "string:{a=1}")

let () =
  let output = Stdlib.Buffer.create 8 in
  let empty_scratch = Bytes.create 0 in
  let add value = Field.add_to_buffer ~scratch:empty_scratch output value in
  expect_invalid (fun () -> add (Field.int min_int));
  expect_invalid (fun () -> add (Field.int64 Int64.min_int));
  expect_invalid (fun () -> add (Field.seq [ Field.int 1 ]));
  expect_invalid (fun () -> add (Field.seq ~dropped:1 []));
  expect_invalid (fun () -> add (Field.map [ ("a", Field.int 1) ]))
