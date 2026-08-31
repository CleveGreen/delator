  $ ./retained.exe
  result=7 effects=2
  $ ./debug_profile.exe
  result=7 effects=1
  $ ./elided.exe
  result=7 effects=0
  $ ./subtyping.exe
  subtyping=ok
  $ ./interface_client.exe
  interface=7

Level flow is checked before static erasure, so every profile rejects the same
invalid source.

  $ rejects () { level=$1; file=$2; message=$3; if DELATOR_TEST_STATIC_LEVEL="$level" ocamlc -stop-after typing -ppx "./log_value_driver.exe --as-ppx" "$file" >"$file.out" 2>&1; then cat "$file.out"; return 1; fi; line=$(grep -F "$message" "$file.out") || { cat "$file.out"; return 1; }; echo "$line" | sed 's/^.*Error: //'; }
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
  $ rejects trace instrumentation_capture.ml 'Trace log-value parameter would be captured by Debug instrumentation'
  Trace log-value parameter would be captured by Debug instrumentation;
