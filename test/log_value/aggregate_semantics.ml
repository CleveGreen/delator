type packet = {
  stable : int;
  mutable trace : int [@log_value.trace];
  mutable debug : int [@log_value.debug];
  info : int [@log_value.info];
}

type event =
  | Event of
      int
      * (int [@log_value.trace])
      * (int [@log_value.debug])
      * (int [@log_value.info])

let effects = ref 0

let mark_effect name value =
  incr effects;
  ignore name;
  value

let trace_sink () (_value : int [@log_value.trace]) = ()

let build () =
  let packet =
    {
      stable = 7;
      trace = (mark_effect "record.trace" 11 [@log_value.trace]);
      debug = (mark_effect "record.debug" 13 [@log_value.debug]);
      info = (mark_effect "record.info" 17 [@log_value.info]);
    }
  in
  packet.trace <- (mark_effect "update.trace" 19 [@log_value.trace]);
  packet.debug <- (mark_effect "update.debug" 23 [@log_value.debug]);
  let event =
    Event
      ( packet.stable,
        (mark_effect "variant.trace" 29 [@log_value.trace]),
        (mark_effect "variant.debug" 31 [@log_value.debug]),
        (mark_effect "variant.info" 37 [@log_value.info]) )
  in
  let tuple =
    ( packet.stable,
      (mark_effect "tuple.trace" 41 [@log_value.trace]),
      (mark_effect "tuple.debug" 43 [@log_value.debug]),
      (mark_effect "tuple.info" 47 [@log_value.info]) )
  in
  let { stable; trace = (_ [@log_value.trace]); debug = (_ [@log_value.debug]); info = (_ [@log_value.info]) } = packet in
  let event_stable =
    match event with
    | Event
        ( event_stable,
          (_ [@log_value.trace]),
          (_ [@log_value.debug]),
          (_ [@log_value.info]) ) ->
        (match packet with
        | { stable = packet_stable; trace = (_ [@log_value.trace]); debug = (_ [@log_value.debug]); info = (_ [@log_value.info]) } ->
            event_stable + packet_stable)
  in
  let (tuple_stable, (_ [@log_value.trace]), (_ [@log_value.debug]), (_ [@log_value.info])) = tuple in
  trace_sink () (packet.info [@log_value.trace]);
  stable + event_stable + tuple_stable

let () =
  let result = build () in
  Printf.printf "aggregate=%d effects=%d\n" result !effects
