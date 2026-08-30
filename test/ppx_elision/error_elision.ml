let untouched value = value + 1
[@@delator.instrument] [@@delator.level debug]

let () = assert (untouched 1 = 2)
