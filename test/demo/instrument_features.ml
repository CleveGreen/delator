let pp_int formatter value = Format.fprintf formatter "n%d" value

let rec even (value [@delator.field.pp pp_int]) =
  if value = 0 then true else odd (value - 1)
[@@delator.instrument]

and odd value =
  if value = 0 then false else even (value - 1)
[@@delator.instrument] [@@delator.skip_all]

let labelled ~left ?(right = 1) value = left + right + value
[@@delator.instrument]

let classify = function
  | 0 -> "zero"
  | _ -> "other"
[@@delator.instrument]

let identity (type a) (value : a) = value
[@@delator.instrument]

let () =
  assert (even 2);
  assert (labelled ~left:1 2 = 4);
  assert (classify 0 = "zero");
  assert (identity 42 = 42)
