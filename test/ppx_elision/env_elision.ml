let touched = ref false

let () =
  [%log.debug "environment-controlled"
    ~field:(touched := true; Delator.Field.string "value")]

let () = if !touched then print_endline "payload-evaluated"
