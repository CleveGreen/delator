  $ ./retained.exe
  result=7 effects=2
  $ ./debug_profile.exe
  result=7 effects=1
  $ ./elided.exe
  result=7 effects=0
  $ ./subtyping.exe
  subtyping=ok
  $ ./semantic_trace.exe
  events=todics ordinary=2 exception=raised shadow=7
  $ ./semantic_debug.exe
  events=odic ordinary=2 exception=raised shadow=7
  $ ./semantic_info.exe
  events=oi ordinary=2 exception=returned shadow=7
  $ ./aggregate_trace.exe
  aggregate=28 effects=11
  $ ./aggregate_debug.exe
  aggregate=28 effects=7
  $ ./aggregate_info.exe
  aggregate=28 effects=3
  $ ./complete_component_trace.exe
  complete=18 effects=4
  $ ./complete_component_debug.exe
  complete=18 effects=2
  $ ./complete_component_info.exe
  complete=18 effects=0
  $ ./application_trace.exe
  direct=30 events=default,direct-debug,direct-trace
  default=31 events=default,default-debug,default-trace
  provided=44 events=provided-debug,provided-tag,provided-trace
  partial=32 events=default,partial-debug,partial-trace
  higher=66 events=default,default,higher-debug,higher-trace
  optional-supplied=50 events=optional-trace
  optional-omitted=51 events=
  $ ./application_debug.exe
  direct=30 events=default,direct-debug
  default=31 events=default,default-debug
  provided=44 events=provided-debug,provided-tag
  partial=32 events=default,partial-debug
  higher=66 events=default,default,higher-debug
  optional-supplied=50 events=
  optional-omitted=51 events=
  $ ./application_info.exe
  direct=30 events=default
  default=31 events=default
  provided=44 events=provided-tag
  partial=32 events=default
  higher=66 events=default,default
  optional-supplied=50 events=
  optional-omitted=51 events=
  $ ./interface_client.exe
  interface=7
  $ ./matrix_checks.exe ./log_value_driver.exe
  lattice=25
  trace=5
  debug=4
  info=3
  warn=2
  error=1
  $ ./adversarial_checks.exe "$(command -v ocamlc)" ./log_value_driver.exe
  adversarial=381

Level flow is checked before static erasure, so every profile rejects the same
invalid source.

  $ rejects () { level=$1; file=$2; message=$3; if DELATOR_TEST_STATIC_LEVEL="$level" ocamlc -stop-after typing -ppx "./log_value_driver.exe --as-ppx" "$file" >"$file.out" 2>&1; then cat "$file.out"; return 1; fi; grep -F "$message" "$file.out" >/dev/null || { cat "$file.out"; return 1; }; printf '%s\n' "$message"; }
  $ rejects trace flow_trace_in_debug.ml 'Trace log-value used in a Debug context'
  Trace log-value used in a Debug context
  $ rejects error flow_trace_in_debug.ml 'Trace log-value used in a Debug context'
  Trace log-value used in a Debug context
  $ rejects trace flow_debug_in_info.ml 'Debug log-value used in an Info context'
  Debug log-value used in an Info context
  $ rejects trace bare_use.ml 'trace_value requires [@log_value.trace] at each use'
  trace_value requires [@log_value.trace] at each use
  $ rejects trace relabel_use.ml 'trace_value is available at trace and cannot be used at debug'
  trace_value is available at trace and cannot be used at debug
  $ rejects trace formal_mismatch.ml 'consume expects [@log_value.trace], not [@log_value.debug]'
  consume expects [@log_value.trace], not [@log_value.debug]
  $ rejects trace missing_actual_annotation.ml 'consume requires [@log_value.trace] on this actual'
  consume requires [@log_value.trace] on this actual
  $ rejects trace field_mismatch.ml 'field metadata expects [@log_value.trace], not [@log_value.debug]'
  field metadata expects [@log_value.trace], not [@log_value.debug]
  $ rejects trace missing_field_annotation.ml 'field metadata requires a compatible [@log_value.LEVEL] use annotation'
  field metadata requires a compatible [@log_value.LEVEL] use annotation
  $ rejects trace fully_gated_function.ml 'a level-gated function must retain at least one ordinary formal'
  a level-gated function must retain at least one ordinary formal
  $ rejects trace fully_gated_signature.mli 'a level-gated function must retain at least one ordinary formal'
  a level-gated function must retain at least one ordinary formal
  $ rejects trace recursive_binding.ml 'a level-gated let must be one nonrecursive binding'
  a level-gated let must be one nonrecursive binding
  $ rejects trace grouped_binding.ml 'a level-gated let must be one nonrecursive binding'
  a level-gated let must be one nonrecursive binding
  $ rejects debug fully_gated_record.ml 'a record cannot contain only level-gated fields'
  a record cannot contain only level-gated fields
  $ rejects trace instrumentation_capture.ml 'Trace log-value parameter would be captured by Debug instrumentation'
  Trace log-value parameter would be captured by Debug instrumentation
