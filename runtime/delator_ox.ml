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

let[@inline always] in_span ~level ~target ~name
    ?(fields : (unit -> Field.t list) @ local = fun () -> [])
    ?(log_exn = true) (action @ local) =
  if not (Runtime.is_enabled ~level ~target) then action ()
  else
    let span = Runtime.new_span ~target ~level ~name ~fields:(fields ()) in
    Runtime.enter span;
    match action () with
    | result -> Runtime.exit span; result
    | exception error ->
        let backtrace = Printexc.get_raw_backtrace () in
        if log_exn then
          Runtime.event ~target ~level:Error ~msg:"uncaught exception"
            ~fields:[ ("exception", Field.exn error) ];
        Runtime.exit span;
        Printexc.raise_with_backtrace error backtrace

let () = init ()
