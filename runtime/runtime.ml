type span = Span.t

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
  let span = Span.create ~name ~target ~level fields in
  Renderer.on_new_span ~id:(Span.id span) ~parent:(Span.parent span) ~name
    ~target ~level ~fields;
  span

let enter span =
  Span.enter span;
  Renderer.on_enter ~id:(Span.id span)

let exit span =
  let duration_ns = Span.exit span in
  Renderer.on_exit ~id:(Span.id span) ~duration_ns;
  if Span.depth () = 0 then Buffer.flush ()

let () = initialize ()
