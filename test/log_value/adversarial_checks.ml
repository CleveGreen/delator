let levels = [ "trace"; "debug"; "info"; "warn"; "error" ]

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let copy source target =
  let input_channel = open_in_bin source in
  let output_channel = open_out_bin target in
  Fun.protect
    ~finally:(fun () ->
      close_in input_channel;
      close_out output_channel)
    (fun () ->
      let buffer = Bytes.create 4096 in
      let rec loop () =
        match input input_channel buffer 0 (Bytes.length buffer) with
        | 0 -> ()
        | count ->
            output output_channel buffer 0 count;
            loop ()
      in
      loop ())

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= text_length
    &&
    (String.sub text offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let environment level =
  let prefix = "DELATOR_TEST_STATIC_LEVEL=" in
  let inherited =
    Unix.environment ()
    |> Array.to_list
    |> List.filter (fun entry ->
           not
             (String.length entry >= String.length prefix
             && String.sub entry 0 (String.length prefix) = prefix))
  in
  Array.of_list ((prefix ^ level) :: inherited)

let run ?level ~output program arguments =
  let descriptor =
    Unix.openfile output [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let process_environment =
    match level with Some level -> environment level | None -> Unix.environment ()
  in
  let pid =
    Unix.create_process_env program (Array.of_list (program :: arguments))
      process_environment Unix.stdin descriptor descriptor
  in
  Unix.close descriptor;
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED status -> status
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let remove_directory directory =
  Sys.readdir directory
  |> Array.iter (fun name -> Sys.remove (Filename.concat directory name));
  Unix.rmdir directory

let with_directory action =
  let directory = Filename.temp_file "delator-adversarial" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  Fun.protect ~finally:(fun () -> remove_directory directory) (fun () -> action directory)

let failures = ref []
let checked = ref 0

let fail format =
  Printf.ksprintf (fun message -> failures := message :: !failures) format

let compile ~ocamlc ~ppx ~level ~output arguments =
  run ~level ~output ocamlc ("-ppx" :: (ppx ^ " --as-ppx") :: arguments)

let check_rejection ~ocamlc ~ppx (source, diagnostic) =
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local = Filename.concat directory (Filename.basename source) in
          let output = Filename.concat directory "compile.out" in
          copy source local;
          let status =
            compile ~ocamlc ~ppx ~level ~output
              [ "-stop-after"; "typing"; local ]
          in
          let message = read output in
          if status = 0 then fail "%s/%s was accepted" source level
          else if not (contains message diagnostic) then
            fail "%s/%s missed diagnostic %S:\n%s" source level diagnostic message;
          incr checked))
    levels

let check_acceptance ~ocamlc ~ppx source =
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local = Filename.concat directory (Filename.basename source) in
          let output = Filename.concat directory "compile.out" in
          copy source local;
          let status =
            compile ~ocamlc ~ppx ~level ~output
              [ "-stop-after"; "typing"; local ]
          in
          if status <> 0 then fail "%s/%s failed:\n%s" source level (read output);
          incr checked))
    levels

let runtime_output source level =
  match (Filename.basename source, level) with
  | ("valid_neighbors.ml" | "valid_tuple_neighbors.ml"), "trace" ->
      "debug\ntrace\nstable\nstable=1 effects=3\n"
  | ("valid_neighbors.ml" | "valid_tuple_neighbors.ml"), "debug" ->
      "debug\nstable\nstable=1 effects=2\n"
  | ("valid_neighbors.ml" | "valid_tuple_neighbors.ml"), _ ->
      "stable\nstable=1 effects=1\n"
  | "unary_variant_gated.ml", _ -> "unary=7\n"
  | "sentinel_partial_runtime.ml", _ -> "done\n"
  | name, level -> invalid_arg (name ^ "/" ^ level)

let check_runtime ~ocamlc ~ppx source =
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local = Filename.concat directory (Filename.basename source) in
          let executable = Filename.concat directory "case.exe" in
          let compile_output = Filename.concat directory "compile.out" in
          let run_output = Filename.concat directory "run.out" in
          copy source local;
          let status =
            compile ~ocamlc ~ppx ~level ~output:compile_output
              [ "-o"; executable; local ]
          in
          if status <> 0 then
            fail "%s/%s failed:\n%s" source level (read compile_output)
          else
            let status = run ~output:run_output executable [] in
            if status <> 0 then
              fail "%s/%s exited %d:\n%s" source level status (read run_output)
            else
              let actual = read run_output in
              let expected = runtime_output source level in
              if actual <> expected then
                fail "%s/%s output %S, expected %S" source level actual expected;
          incr checked))
    levels

let check_interface ~ocamlc ~ppx ~reject stem =
  let source_directory = "adversarial_interfaces" in
  let implementation = Filename.concat source_directory (stem ^ ".ml") in
  let interface = Filename.concat source_directory (stem ^ ".mli") in
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local_ml = Filename.concat directory (stem ^ ".ml") in
          let local_mli = Filename.concat directory (stem ^ ".mli") in
          let standalone_ml = Filename.concat directory "standalone.ml" in
          let precheck = Filename.concat directory "precheck.out" in
          let interface_output = Filename.concat directory "interface.out" in
          let implementation_output = Filename.concat directory "implementation.out" in
          copy implementation local_ml;
          copy implementation standalone_ml;
          copy interface local_mli;
          let standalone source =
            compile ~ocamlc ~ppx ~level ~output:precheck
              [ "-stop-after"; "typing"; source ]
          in
          if standalone local_mli <> 0 then
            fail "%s/%s is not independently valid:\n%s" interface level
              (read precheck)
          else if standalone standalone_ml <> 0 then
            fail "%s/%s is not independently valid:\n%s" implementation level
              (read precheck)
          else
            let interface_status =
              compile ~ocamlc ~ppx ~level ~output:interface_output
                [ "-c"; "-I"; directory; local_mli ]
            in
            if interface_status <> 0 then
              fail "%s/%s failed:\n%s" interface level (read interface_output)
            else
              let implementation_status =
                compile ~ocamlc ~ppx ~level ~output:implementation_output
                  [ "-c"; "-I"; directory; local_ml ]
              in
              if reject && implementation_status = 0 then
                fail "%s/%s implementation drift was accepted" stem level
              else if (not reject) && implementation_status <> 0 then
                fail "%s/%s matching pair failed:\n%s" stem level
                  (read implementation_output)
              else if
                reject
                && not (contains (read implementation_output) "Error:")
              then
                fail "%s/%s drift did not produce a compiler error:\n%s" stem level
                  (read implementation_output);
          incr checked))
    levels

let check_interface_client ~ocamlc ~ppx stem =
  let source_directory = "adversarial_interfaces" in
  let implementation = Filename.concat source_directory (stem ^ ".ml") in
  let interface = Filename.concat source_directory (stem ^ ".mli") in
  let client = Filename.concat source_directory (stem ^ "_client.ml") in
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local_ml = Filename.concat directory (stem ^ ".ml") in
          let local_mli = Filename.concat directory (stem ^ ".mli") in
          let local_client =
            Filename.concat directory (stem ^ "_client.ml")
          in
          let output = Filename.concat directory "compile.out" in
          copy implementation local_ml;
          copy interface local_mli;
          copy client local_client;
          let compile_one source =
            compile ~ocamlc ~ppx ~level ~output
              [ "-c"; "-I"; directory; source ]
          in
          if compile_one local_mli <> 0 then
            fail "%s/%s failed:\n%s" interface level (read output)
          else if compile_one local_ml <> 0 then
            fail "%s/%s failed:\n%s" implementation level (read output)
          else if compile_one local_client <> 0 then
            fail "%s/%s failed:\n%s" client level (read output);
          incr checked))
    levels

let check_interface_client_rejection ~ocamlc ~ppx ~diagnostic stem client_name =
  let source_directory = "adversarial_interfaces" in
  let implementation = Filename.concat source_directory (stem ^ ".ml") in
  let interface = Filename.concat source_directory (stem ^ ".mli") in
  let client = Filename.concat source_directory client_name in
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local_ml = Filename.concat directory (stem ^ ".ml") in
          let local_mli = Filename.concat directory (stem ^ ".mli") in
          let local_client = Filename.concat directory client_name in
          let output = Filename.concat directory "compile.out" in
          copy implementation local_ml;
          copy interface local_mli;
          copy client local_client;
          let compile_one source =
            compile ~ocamlc ~ppx ~level ~output
              [ "-c"; "-I"; directory; source ]
          in
          if compile_one local_mli <> 0 then
            fail "%s/%s failed:\n%s" interface level (read output)
          else if compile_one local_ml <> 0 then
            fail "%s/%s failed:\n%s" implementation level (read output)
          else
            let status = compile_one local_client in
            let message = read output in
            if status = 0 then fail "%s/%s was accepted" client level
            else if not (contains message diagnostic) then
              fail "%s/%s missed diagnostic %S:\n%s" client level diagnostic
                message;
          incr checked))
    [ "trace" ]

let check_provider_rejection ~ocamlc ~ppx ~diagnostics client_name =
  let source_directory = "adversarial_calls" in
  let provider = Filename.concat source_directory "suffix_provider.ml" in
  let client = Filename.concat source_directory client_name in
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local_provider = Filename.concat directory "suffix_provider.ml" in
          let local_client = Filename.concat directory client_name in
          let output = Filename.concat directory "compile.out" in
          copy provider local_provider;
          copy client local_client;
          let compile_one source =
            compile ~ocamlc ~ppx ~level ~output
              [ "-c"; "-I"; directory; source ]
          in
          if compile_one local_provider <> 0 then
            fail "%s/%s failed:\n%s" provider level (read output)
          else
            let status = compile_one local_client in
            let message = read output in
            if status = 0 then fail "%s/%s was accepted" client level
            else
              List.iter
                (fun diagnostic ->
                  if not (contains message diagnostic) then
                    fail "%s/%s missed diagnostic %S:\n%s" client level
                      diagnostic message)
                diagnostics;
          incr checked))
    levels

let check_interface_abi ~ocamlc ~ppx ~marker ~hidden_levels stem =
  let source_directory = "adversarial_interfaces" in
  let implementation = Filename.concat source_directory (stem ^ ".ml") in
  let interface = Filename.concat source_directory (stem ^ ".mli") in
  List.iter
    (fun level ->
      with_directory (fun directory ->
          let local_ml = Filename.concat directory (stem ^ ".ml") in
          let local_mli = Filename.concat directory (stem ^ ".mli") in
          let standalone_ml = Filename.concat directory "standalone.ml" in
          let output = Filename.concat directory "compile.out" in
          copy implementation local_ml;
          copy implementation standalone_ml;
          copy interface local_mli;
          let compile_one arguments =
            compile ~ocamlc ~ppx ~level ~output arguments
          in
          if
            compile_one [ "-stop-after"; "typing"; local_mli ] <> 0
          then fail "%s/%s is not independently valid:\n%s" interface level (read output)
          else if
            compile_one [ "-stop-after"; "typing"; standalone_ml ] <> 0
          then
            fail "%s/%s is not independently valid:\n%s" implementation level
              (read output)
          else if
            compile_one [ "-c"; "-I"; directory; local_mli ] <> 0
          then fail "%s/%s failed:\n%s" interface level (read output)
          else
            let status =
              compile_one [ "-c"; "-I"; directory; local_ml ]
            in
            let message = read output in
            if status = 0 then
              fail "%s/%s aggregate ABI drift was accepted" stem level
            else if not (contains message "does not match the interface") then
              fail "%s/%s was not rejected as interface drift:\n%s" stem level
                message
            else if
              List.mem level hidden_levels && not (contains message marker)
            then
              fail "%s/%s did not expose aggregate marker %S:\n%s" stem level
                marker message;
          incr checked))
    levels

let rejected =
  [ ( "adversarial_calls/qualified_module.ml",
      "cannot authenticate [@log_value.trace] actual" );
    ( "adversarial_calls/local_alias.ml",
      "cannot authenticate [@log_value.trace] actual" );
    ( "adversarial_calls/higher_order_callee.ml",
      "cannot authenticate [@log_value.trace] actual" );
    ( "adversarial_calls/partial_mismatch.ml",
      "consume expects [@log_value.debug], not [@log_value.trace]" );
    ( "adversarial_calls/reordered_labeled.ml",
      "consume expects [@log_value.debug], not [@log_value.trace]" );
    ( "adversarial_calls/partial_suffix_timing.ml",
      "partial application of f leaves only level-gated" );
    ( "adversarial_variants/positional_variant_drift.ml",
      "constructor Payload component 3 is not level-gated" );
    ( "adversarial_variants/positional_tuple_drift.ml",
      "tuple component 3 is not level-gated" );
    ( "adversarial_variants/missing_variant_use.ml",
      "constructor Payload component 2 requires [@log_value.trace]" );
    ( "adversarial_variants/wrong_variant_use.ml",
      "constructor Payload[2] expects [@log_value.trace]" );
    ( "adversarial_variants/misplaced_tuple_use.ml",
      "tuple component 1 is not level-gated" );
    ( "adversarial_variants/untyped_tuple_drift.ml",
      "a gated tuple site requires an explicit named contract" );
    ( "adversarial_variants/lexical_tuple_escape.ml",
      "tuple value value has level-gated lexical provenance" );
    ( "adversarial_variants/unary_variant_missing_use.ml",
      "constructor Payload component 1 requires [@log_value.trace]" );
    ( "adversarial_patterns_records/duplicate_record_labels.ml",
      "record field shared has ambiguous log-value declarations" );
    ( "adversarial_patterns_records/ordinary_field_mutation.ml",
      "field count is not level-gated, but this site uses [@log_value.trace]" );
    ( "adversarial_patterns_records/refutable_gated_pattern.ml",
      "a level-gated pattern component must be irrefutable" );
    ( "adversarial_patterns_records/fully_gated_anonymous_function.ml",
      "a level-gated function must retain at least one ordinary formal" );
    ( "adversarial_patterns_records/fully_gated_returned_function.ml",
      "a level-gated function must retain at least one ordinary formal" ) ]

let accepted =
  [ "adversarial_calls/qualified_positive.ml";
    "adversarial_variants/ordinary_pair_control.ml";
    "adversarial_variants/lexical_tuple_alias.ml";
    "adversarial_variants/named_tuple_escape_control.ml";
    "adversarial_patterns_records/duplicate_record_labels_control.ml";
    "adversarial_patterns_records/ordinary_field_mutation_control.ml";
    "adversarial_patterns_records/refutable_gated_pattern_control.ml";
    "adversarial_patterns_records/fully_gated_anonymous_function_control.ml";
    "adversarial_patterns_records/fully_gated_returned_function_control.ml" ]

let runtime =
  [ "adversarial_variants/valid_neighbors.ml";
    "adversarial_variants/valid_tuple_neighbors.ml";
    "adversarial_variants/unary_variant_gated.ml";
    "adversarial_calls/sentinel_partial_runtime.ml" ]

let () =
  if Array.length Sys.argv <> 3 then
    invalid_arg "adversarial_checks.exe OCAMLC PPX_DRIVER";
  let ocamlc = absolute Sys.argv.(1) in
  let ppx = absolute Sys.argv.(2) in
  List.iter (check_rejection ~ocamlc ~ppx) rejected;
  List.iter (check_acceptance ~ocamlc ~ppx) accepted;
  List.iter (check_runtime ~ocamlc ~ppx) runtime;
  List.iter (check_interface ~ocamlc ~ppx ~reject:false)
    [ "matching"; "record_matching" ];
  List.iter (check_interface ~ocamlc ~ppx ~reject:true)
    [ "reordered"; "mismatched"; "three_levels"; "record_drift" ];
  check_interface_client ~ocamlc ~ppx "subtyping";
  check_interface_client_rejection ~ocamlc ~ppx
    ~diagnostic:"Delator_log_value_debug" "subtyping"
    "subtyping_bad_client.ml";
  List.iter
    (fun (client, diagnostics) ->
      check_provider_rejection ~ocamlc ~ppx ~diagnostics client)
    [ ( "suffix_shadow_call.ml",
        [ "delator_internal_log_value_function_636f6e73756d65";
          "Delator_log_value_trace" ] );
      ( "suffix_shadow_value.ml",
        [ "delator_internal_log_value_value_76616c7565";
          "Delator_log_value_trace" ] );
      ( "suffix_shadow_field.ml",
        [ "delator_internal_log_value_field_6d65746164617461";
          "Delator_log_value_trace" ] );
      ( "suffix_shadow_constructor.ml",
        [ "delator_internal_log_value_constructor_5061796c6f6164";
          "Delator_log_value_trace" ] );
      ( "suffix_future_rebind.ml",
        [ "cannot authenticate [@log_value.trace] actual" ] ) ];
  List.iter
    (fun (stem, marker, hidden_levels) ->
      check_interface_abi ~ocamlc ~ppx ~marker ~hidden_levels stem)
    [ ( "function_extra",
        "delator_internal_log_value_function_abi_66",
        [ "debug"; "info"; "warn"; "error" ] );
      ( "record_extra",
        "delator_internal_log_value_type_abi_74",
        [ "debug"; "info"; "warn"; "error" ] );
      ( "variant_extra",
        "delator_internal_log_value_type_abi_74",
        [ "debug"; "info"; "warn"; "error" ] );
      ( "optional_label_drift",
        "delator_internal_log_value_function_abi_66",
        [ "debug"; "info"; "warn"; "error" ] );
      ( "interleaved_slots",
        "delator_internal_log_value_function_abi_66",
        [ "error" ] ) ];
  match List.rev !failures with
  | [] -> Printf.printf "adversarial=%d\n" !checked
  | failures ->
      List.iter (Printf.eprintf "FAIL: %s\n") failures;
      exit 1
