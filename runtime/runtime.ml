type span = {
  core : Span.t;
  target : string;
  level : Level.t;
  name : string;
  fields : Field.t list;
}

let initialized = ref false
let initialization_lock = Mutex.create ()

let initialize () =
  Mutex.lock initialization_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock initialization_lock)
    (fun () ->
      if not !initialized then begin
        Clock.configure_from_env ();
        Filter.configure_from_env ();
        Buffer.configure_from_env ();
        Level.configure_color_from_env ();
        Renderer.configure_from_env ();
        Buffer.install_at_exit ();
        initialized := true
      end)

let is_enabled ~level ~target =
  Filter.is_enabled ~level ~target

let[@inline always] event ~target ~level ~msg ~fields =
  Renderer.on_event ~span:(Span.current_id ()) ~target ~level ~msg ~fields

let new_span ~target ~level ~name ~fields =
  { core = Span.create (); target; level; name; fields }

let enter span =
  Span.enter span.core;
  match
    Renderer.on_new_span ~id:(Span.id span.core) ~parent:(Span.parent span.core)
      ~name:span.name ~target:span.target ~level:span.level ~fields:span.fields
  with
  | () -> ()
  | exception error ->
      let backtrace = Printexc.get_raw_backtrace () in
      ignore (Span.exit span.core : int64);
      Printexc.raise_with_backtrace error backtrace

let exit span =
  let duration_ns = Span.exit span.core in
  Renderer.on_exit ~id:(Span.id span.core) ~duration_ns

let () = initialize ()
