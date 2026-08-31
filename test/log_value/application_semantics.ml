let events = ref []

let mark name value =
  events := !events @ [ name ];
  value

let reset () = events := []

let show name value =
  Printf.printf "%s=%d events=%s\n" name value
    (String.concat "," (List.sort String.compare !events))

let consume ?(tag = mark "default" 20)
    ~trace:(_trace : int [@log_value.trace])
    ~debug:(_debug : int [@log_value.debug]) value =
  [%log.trace "consume"
    ~trace:(Delator.Field.int (_trace [@log_value.trace]))
    ~debug:(Delator.Field.int (_debug [@log_value.debug]))
  ];
  value + tag

let direct () =
  reset ();
  let result =
    consume
      ~trace:(mark "direct-trace" 1 [@log_value.trace])
      ~debug:(mark "direct-debug" 2 [@log_value.debug])
      10
  in
  show "direct" result

let default () =
  reset ();
  let result =
    consume
      ~trace:(mark "default-trace" 3 [@log_value.trace])
      ~debug:(mark "default-debug" 4 [@log_value.debug])
      11
  in
  show "default" result

let provided () =
  reset ();
  let result =
    consume
      ~tag:(mark "provided-tag" 30)
      ~trace:(mark "provided-trace" 7 [@log_value.trace])
      ~debug:(mark "provided-debug" 8 [@log_value.debug])
      14
  in
  show "provided" result

let partial () =
  reset ();
  let apply_later =
    consume
      ~trace:(mark "partial-trace" 5 [@log_value.trace])
      ~debug:(mark "partial-debug" 6 [@log_value.debug])
  in
  let result = apply_later 12 in
  show "partial" result

let apply_twice function_ value = function_ value + function_ value

let higher_order () =
  reset ();
  let result =
    apply_twice
      (consume
         ~trace:(mark "higher-trace" 5 [@log_value.trace])
         ~debug:(mark "higher-debug" 6 [@log_value.debug]))
      13
  in
  show "higher" result

let consume_optional ?(trace : int option [@log_value.trace]) value =
  [%log.trace "optional"
    ~trace:
      (Delator.Field.int
         (Option.value ~default:0 (trace [@log_value.trace])))];
  value

let optional_supplied () =
  reset ();
  let result =
    consume_optional ~trace:(mark "optional-trace" 9 [@log_value.trace]) 50
  in
  show "optional-supplied" result

let optional_omitted () =
  reset ();
  let result = consume_optional 51 in
  show "optional-omitted" result

let () =
  direct ();
  default ();
  provided ();
  partial ();
  higher_order ();
  optional_supplied ();
  optional_omitted ()
