module type S = Renderer_intf.S

let tree = (module Renderer_tree : S)
let flat = (module Renderer_flat : S)
let current = ref tree
let set_current renderer = current := renderer

let configure_from_env () =
  match Sys.getenv_opt "DELATOR_FORMAT" with
  | None | Some "" | Some "tree" -> set_current tree
  | Some "flat" -> set_current flat
  | Some value -> invalid_arg (Printf.sprintf "DELATOR_FORMAT: unknown format %S" value)

let on_new_span ~id ~parent ~name ~target ~level ~fields =
  let module Current = (val !current : S) in
  Current.on_new_span ~id ~parent ~name ~target ~level ~fields

let on_enter ~id =
  let module Current = (val !current : S) in
  Current.on_enter ~id

let on_exit ~id ~duration_ns =
  let module Current = (val !current : S) in
  Current.on_exit ~id ~duration_ns

let on_event ~span ~target ~level ~msg ~fields =
  let module Current = (val !current : S) in
  Current.on_event ~span ~target ~level ~msg ~fields
