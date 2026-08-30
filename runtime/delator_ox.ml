module Raw_level = Level
module type Raw_level_signature = module type of Raw_level
module type Portable_level_signature = sig @@ portable
  include module type of Raw_level
end

(* These casts expose the documented shared runtime contract to OxCaml's mode
   checker without wrapping any operation. [Obj.magic] and
   [Obj.magic_portable] are representation-preserving compiler primitives. *)
module Level : Portable_level_signature =
  (val Obj.magic_portable
      (Obj.magic
         (module Raw_level : Raw_level_signature)
       : (module Portable_level_signature))
   : Portable_level_signature)

module Raw_field = Field
module type Raw_field_signature = module type of Raw_field
module type Portable_field_signature = sig @@ portable
  include module type of Raw_field
end
module Field : Portable_field_signature =
  (val Obj.magic_portable
      (Obj.magic
         (module Raw_field : Raw_field_signature)
       : (module Portable_field_signature))
   : Portable_field_signature)

module Raw_renderer = Renderer
module type Raw_renderer_signature = module type of Raw_renderer
module type Portable_renderer_signature = sig @@ portable
  include module type of Raw_renderer
end
module Renderer : Portable_renderer_signature =
  (val Obj.magic_portable
      (Obj.magic
         (module Raw_renderer : Raw_renderer_signature)
       : (module Portable_renderer_signature))
   : Portable_renderer_signature)

module Color = struct
  let is_enabled = Level.is_color_enabled
  let set_enabled = Level.set_color_enabled
end

module Raw_clock = Clock
module type Raw_clock_signature = module type of Raw_clock
module type Portable_clock_signature = sig @@ portable
  include module type of Raw_clock
end
module Clock : Portable_clock_signature =
  (val Obj.magic_portable
      (Obj.magic
         (module Raw_clock : Raw_clock_signature)
       : (module Portable_clock_signature))
   : Portable_clock_signature)

module Raw_runtime = Runtime
module type Raw_runtime_signature = module type of Raw_runtime
module type Portable_runtime_signature = sig @@ portable
  include module type of Raw_runtime
end
module Runtime : Portable_runtime_signature =
  (val Obj.magic_portable
      (Obj.magic
         (module Raw_runtime : Raw_runtime_signature)
       : (module Portable_runtime_signature))
   : Portable_runtime_signature)

type level = Level.t = Trace | Debug | Info | Warn | Error

let init = Runtime.initialize

let set_default_level = Obj.magic_portable Filter.set_default

let start_span ~target ~level ~name ~fields =
  let context = Span.current_context () in
  let parent = Span.current_id_in context in
  let id = Span.start context in
  Span.enter_started context id;
  match Renderer.on_new_span ~id ~parent ~name ~target ~level ~fields with
  | () -> id
  | exception error ->
      let backtrace = Printexc.get_raw_backtrace () in
      ignore (Span.finish id : int64);
      Printexc.raise_with_backtrace error backtrace

let exit_span id =
  let duration_ns = Span.finish id in
  Renderer.on_exit ~id ~duration_ns

let[@inline always] in_span_nonportable ~level ~target ~name
    ?(fields : (unit -> Field.t list) @ local = fun () -> [])
    ?(log_exn = true) (action @ local) =
  if not (Runtime.is_enabled ~level ~target) then action ()
  else
    let span = start_span ~target ~level ~name ~fields:(fields ()) in
    match action () with
    | result -> exit_span span; result
    | exception error ->
        let backtrace = Printexc.get_raw_backtrace () in
        if log_exn then
          Runtime.event ~target ~level:Error ~msg:"uncaught exception"
            ~fields:[ ("exception", Field.exn error) ];
        exit_span span;
        Printexc.raise_with_backtrace error backtrace

let in_span = Obj.magic_portable in_span_nonportable

let () = init ()
