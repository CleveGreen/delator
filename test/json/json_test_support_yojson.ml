[@@@alert "-unsafe_multidomain"]
[@@@alert "-do_not_spawn_domains"]

let read_all descriptor =
  let input = Unix.in_channel_of_descr descriptor in
  let rec loop lines =
    match input_line input with
    | line -> loop (line :: lines)
    | exception End_of_file -> List.rev lines
  in
  let lines = loop [] in
  close_in input;
  lines

let capture render =
  let read_descriptor, write_descriptor = Unix.pipe () in
  let stderr_descriptor = Unix.descr_of_out_channel stderr in
  let saved_stderr = Unix.dup stderr_descriptor in
  Unix.dup2 write_descriptor stderr_descriptor;
  Unix.close write_descriptor;
  Fun.protect
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 saved_stderr stderr_descriptor;
      Unix.close saved_stderr)
    render;
  read_all read_descriptor

let member name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> failwith "expected JSON object"

let string_member name json =
  match member name json with
  | `String value -> value
  | _ -> failwith ("expected JSON string member " ^ name)

let optional_int_member name json =
  match member name json with
  | `Null -> None
  | `Int value -> Some value
  | _ -> failwith ("expected nullable JSON integer member " ^ name)

let find_record ~kind ~message records =
  List.find
    (fun json ->
      String.equal (string_member "kind" json) kind
      && String.equal (string_member "message" json) message)
    records

let test_schema (module Json : Delator.Renderer.S) =
  let lines =
    capture (fun () ->
        Json.on_new_span ~id:7 ~parent:None ~name:"compile\nunit"
          ~target:"machine\"target" ~level:Delator.Info
          ~fields:
            [ ("text", Delator.Field.string "quoted\tvalue");
              ("count", Delator.Field.int 3);
              ("cached", Delator.Field.bool true) ];
        Json.on_event ~span:(Some 7) ~target:"machine\"target"
          ~level:Delator.Warn ~msg:"event\\message"
          ~fields:[ "ok", Delator.Field.bool false ];
        Json.on_exit ~id:7 ~duration_ns:42L)
  in
  let json = List.map Yojson.Safe.from_string lines in
  let expected =
    [ `Assoc
        [ ("kind", `String "span_start");
          ("target", `String "machine\"target");
          ("span_id", `Int 7);
          ("parent_span_id", `Null);
          ("level", `String "INFO");
          ("message", `String "compile\nunit");
          ( "fields",
            `Assoc
              [ ("text", `String "quoted\tvalue");
                ("count", `Int 3);
                ("cached", `Bool true) ] ) ];
      `Assoc
        [ ("kind", `String "event");
          ("target", `String "machine\"target");
          ("span_id", `Int 7);
          ("parent_span_id", `Null);
          ("level", `String "WARN");
          ("message", `String "event\\message");
          ("fields", `Assoc [ "ok", `Bool false ]) ];
      `Assoc
        [ ("kind", `String "span_end");
          ("target", `String "machine\"target");
          ("span_id", `Int 7);
          ("parent_span_id", `Null);
          ("level", `String "INFO");
          ("message", `String "compile\nunit");
          ("fields", `Assoc []);
          ("duration_ns", `Int 42) ] ]
  in
  assert (json = expected)

type gate = {
  mutex : Mutex.t;
  condition : Condition.t;
  mutable stage : int;
}

let create_gate () =
  { mutex = Mutex.create (); condition = Condition.create (); stage = 0 }

let set_stage gate stage =
  Mutex.lock gate.mutex;
  gate.stage <- stage;
  Condition.broadcast gate.condition;
  Mutex.unlock gate.mutex

let wait_for_stage gate stage =
  Mutex.lock gate.mutex;
  while gate.stage < stage do
    Condition.wait gate.condition gate.mutex
  done;
  Mutex.unlock gate.mutex

let event message =
  Delator.Runtime.event ~target:"domain-isolation" ~level:Delator.Trace ~msg:message
    ~fields:[]

let test_domain_isolation (module Json : Delator.Renderer.S) =
  Delator.Renderer.set_current (module Json);
  Delator.Clock.disable ();
  Delator.set_default_level Delator.Trace;
  let gate = create_gate () in
  let records =
    capture (fun () ->
        Delator.in_span ~level:Delator.Trace ~target:"domain-isolation" ~name:"x"
          (fun () ->
            let child =
              Domain.spawn (fun () ->
                  Delator.in_span ~level:Delator.Trace
                    ~target:"domain-isolation" ~name:"y" (fun () ->
                      set_stage gate 1;
                      wait_for_stage gate 2;
                      event "child-in-y"))
            in
            wait_for_stage gate 1;
            let observer = Domain.spawn (fun () -> event "independent-observer") in
            Domain.join observer;
            event "parent-while-y-active";
            set_stage gate 2;
            Domain.join child;
            event "parent-after-y"))
    |> List.map Yojson.Safe.from_string
  in
  assert (List.length records = 8);
  let x = find_record ~kind:"span_start" ~message:"x" records in
  let y = find_record ~kind:"span_start" ~message:"y" records in
  let x_id = Option.get (optional_int_member "span_id" x) in
  let y_id = Option.get (optional_int_member "span_id" y) in
  assert (x_id <> y_id);
  assert (optional_int_member "parent_span_id" x = None);
  assert (optional_int_member "parent_span_id" y = None);
  let parent_during =
    find_record ~kind:"event" ~message:"parent-while-y-active" records
  in
  let parent_after = find_record ~kind:"event" ~message:"parent-after-y" records in
  let child = find_record ~kind:"event" ~message:"child-in-y" records in
  let observer =
    find_record ~kind:"event" ~message:"independent-observer" records
  in
  assert (optional_int_member "span_id" parent_during = Some x_id);
  assert (optional_int_member "span_id" parent_after = Some x_id);
  assert (optional_int_member "span_id" child = Some y_id);
  assert (optional_int_member "span_id" observer = None);
  List.iter
    (fun record -> assert (optional_int_member "parent_span_id" record = None))
    [ parent_during; parent_after; child; observer ]

let run () =
  let renderer = Option.get Delator.Renderer.json in
  test_schema renderer;
  for _ = 1 to 32 do
    test_domain_isolation renderer
  done
