exception Broken

let fail () = raise Broken [@@delator.instrument]

let quiet_fail () = raise Broken
[@@delator.instrument] [@@delator.no_exn_log]

let () =
  (try fail () with Broken -> ());
  try quiet_fail () with Broken -> ()
