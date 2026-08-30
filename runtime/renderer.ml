module type S = Renderer_intf.S

let tree = (module Renderer_tree : S)
let flat = (module Renderer_flat : S)

type callbacks = {
  on_new_span :
    id:int -> parent:int option -> name:string -> target:string ->
    level:Level.t -> fields:Field.t list -> unit;
  on_exit : id:int -> duration_ns:int64 -> unit;
  on_event :
    span:int option -> target:string -> level:Level.t -> msg:string ->
    fields:Field.t list -> unit;
}

let callbacks (module Renderer : S) =
  { on_new_span = Renderer.on_new_span;
    on_exit = Renderer.on_exit;
    on_event = Renderer.on_event }

(* Unpack the first-class module when configuration changes, not on every log
   operation. The callbacks remain replaceable for applications and tests. *)
let current = ref (callbacks tree)
let set_current renderer = current := callbacks renderer

let configure_from_env () =
  match Sys.getenv_opt "DELATOR_FORMAT" with
  | None | Some "" | Some "tree" -> set_current tree
  | Some "flat" -> set_current flat
  | Some value -> invalid_arg (Printf.sprintf "DELATOR_FORMAT: unknown format %S" value)

let on_new_span ~id ~parent ~name ~target ~level ~fields =
  (!current).on_new_span ~id ~parent ~name ~target ~level ~fields

let on_exit ~id ~duration_ns =
  (!current).on_exit ~id ~duration_ns

let on_event ~span ~target ~level ~msg ~fields =
  (!current).on_event ~span ~target ~level ~msg ~fields
