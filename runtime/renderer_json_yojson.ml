(* The stack is created and accessed only by its owning domain. *)
[@@@alert "-unsafe_multidomain"]

type span = {
  id : int;
  parent : int option;
  name : string;
  target : string;
  level : Level.t;
}

type state = { mutable stack : span list }

let state = Domain.DLS.new_key (fun () -> { stack = [] })

let json_of_option json_of_value = function
  | None -> `Null
  | Some value -> json_of_value value

let json_of_float value =
  match classify_float value with
  | FP_nan -> `String "NaN"
  | FP_infinite -> `String (if value > 0. then "Infinity" else "-Infinity")
  | FP_zero | FP_normal | FP_subnormal -> `Float value

let rec json_of_value value =
  match Field.view value with
  | Field.View.Null -> `Null
  | Field.View.Bool value -> `Bool value
  | Field.View.Int value -> `Int value
  | Field.View.Int64 value -> `Intlit (Int64.to_string value)
  | Field.View.Float value -> json_of_float value
  | Field.View.String value -> `String value
  | Field.View.Seq { shown; dropped } ->
      let shown = `List (List.map json_of_value shown) in
      if dropped = 0 then shown
      else `Assoc [ ("shown", shown); ("dropped", `Int dropped) ]
  | Field.View.Map { shown; dropped } ->
      let shown = `Assoc (List.map json_of_field shown) in
      if dropped = 0 then shown
      else `Assoc [ ("shown", shown); ("dropped", `Int dropped) ]

and json_of_field (name, value) = (name, json_of_value value)

let json_of_fields fields = `Assoc (List.map json_of_field fields)

let emit json =
  let line = Buffer.line_buffer () in
  Yojson.Safe.to_buffer ~std:true ~suf:"" (Buffer.output line) json;
  Buffer.finish_line line

let common ~kind ~target ~span_id ~parent_span_id ~level ~message ~fields =
  [ ("kind", `String kind);
    ("target", json_of_option (fun value -> `String value) target);
    ("span_id", json_of_option (fun value -> `Int value) span_id);
    ("parent_span_id", json_of_option (fun value -> `Int value) parent_span_id);
    ("level", json_of_option (fun value -> `String (Level.to_string value)) level);
    ("message", json_of_option (fun value -> `String value) message);
    ("fields", json_of_fields fields) ]

let on_new_span ~id ~parent ~name ~target ~level ~fields =
  let state = Domain.DLS.get state in
  emit
    (`Assoc
      (common ~kind:"span_start" ~target:(Some target) ~span_id:(Some id)
         ~parent_span_id:parent ~level:(Some level) ~message:(Some name) ~fields));
  state.stack <- { id; parent; name; target; level } :: state.stack

let on_exit ~id ~duration_ns =
  let state = Domain.DLS.get state in
  let span =
    match state.stack with
    | span :: rest when span.id = id ->
        state.stack <- rest;
        Some span
    | _ -> None
  in
  let target, parent_span_id, level, message =
    match span with
    | Some span ->
        (Some span.target, span.parent, Some span.level, Some span.name)
    | None -> (None, None, None, None)
  in
  emit
    (`Assoc
      (common ~kind:"span_end" ~target ~span_id:(Some id) ~parent_span_id
         ~level ~message ~fields:[]
      @ [ "duration_ns", `Intlit (Int64.to_string duration_ns) ]));
  if Span.depth () = 0 then Buffer.flush ()

let on_event ~span ~target ~level ~msg ~fields =
  let state = Domain.DLS.get state in
  let parent_span_id =
    match state.stack with
    | current :: _ when Some current.id = span -> current.parent
    | _ -> None
  in
  emit
    (`Assoc
      (common ~kind:"event" ~target:(Some target) ~span_id:span
         ~parent_span_id ~level:(Some level) ~message:(Some msg) ~fields))

let renderer =
  Some
    (module struct
      let on_new_span = on_new_span
      let on_exit = on_exit
      let on_event = on_event
    end : Renderer_intf.S)
