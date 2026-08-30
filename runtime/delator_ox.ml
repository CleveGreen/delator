module Level = Level
module Field = Field
module Renderer = Renderer
module Color = struct
  let is_enabled = Level.is_color_enabled
  let set_enabled = Level.set_color_enabled
end
module Clock = Clock
module Runtime = Runtime

type level = Level.t = Trace | Debug | Info | Warn | Error

let init = Runtime.initialize

let set_default_level = Filter.set_default

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

let[@inline always] in_span ~level ~target ~name
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

let () = init ()
