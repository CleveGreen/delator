let leaf (node [@delator.field string_of_int]) =
  [%log.info "rewrite node" ~node:(Delator.Field.int node)]
[@@delator.instrument]

let pass = "simplify"

let lower (unit_name [@delator.field Fun.id]) =
  [%log.debug "select pass" ~pass];
  leaf 7;
  [%delator.warn "fallback" ~unit_name]
[@@delator.instrument]

let () = lower "Demo"
