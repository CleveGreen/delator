type packet = {
  run : int;
  mutable trace : int [@log_value.trace];
  mutable debug : int [@log_value.debug];
}

type choice =
  | C of int * (int [@log_value.trace]) * (int [@log_value.debug])

let effects = ref 0

let _metadata value =
  incr effects;
  value

let consume
    (trace : int [@log_value.trace])
    (debug : int [@log_value.debug])
    run =
  [%log.trace "trace" ~trace:(Delator.Field.int (trace [@log_value.trace]))];
  [%log.debug "debug" ~debug:(Delator.Field.int (debug [@log_value.debug]))];
  run

let update
    _packet
    (trace : int [@log_value.trace])
    (debug : int [@log_value.debug]) =
  _packet.trace <- (trace [@log_value.trace]);
  _packet.debug <- (debug [@log_value.debug])

let run run =
  let[@log_value.trace] trace = _metadata 11 in
  let[@log_value.debug] debug = _metadata 13 in
  let packet =
    {
      run;
      trace = (trace [@log_value.trace]);
      debug = (debug [@log_value.debug]);
    }
  in
  let choice =
    C
      ( run,
        (trace [@log_value.trace]),
        (debug [@log_value.debug]) )
  in
  update
    packet
    (trace [@log_value.trace])
    (debug [@log_value.debug]);
  let
    {
      run = _packet_run;
      trace = (_ [@log_value.trace]);
      debug = (_ [@log_value.debug]);
    } = packet
  in
  let C
        ( choice_run,
          (_ [@log_value.trace]),
          (_ [@log_value.debug]) ) = choice
  in
  consume
    (packet.trace [@log_value.trace])
    (packet.debug [@log_value.debug])
    choice_run

let () =
  let result = run 7 in
  Printf.printf "result=%d effects=%d\n" result !effects
