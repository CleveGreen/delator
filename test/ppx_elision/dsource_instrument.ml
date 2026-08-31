let untouched value = value + 1
[@@delator.instrument] [@@delator.level debug]

let untouched_field (value [@delator.field string_of_int]) = value + 1
[@@delator.instrument] [@@delator.level debug]

let untouched_field_pp (value [@delator.field.pp Format.pp_print_int]) =
  value + 1
[@@delator.instrument] [@@delator.level debug]

let untouched_skip (value [@delator.skip]) = value + 1
[@@delator.instrument] [@@delator.level debug]
