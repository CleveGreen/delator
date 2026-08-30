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

let run () =
  let module Json = (val Option.get Delator.Renderer.json) in
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
