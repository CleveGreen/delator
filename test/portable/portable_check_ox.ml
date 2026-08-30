module Null_renderer : Delator.Renderer.S = struct
  let on_new_span ~id:_ ~parent:_ ~name:_ ~target:_ ~level:_ ~fields:_ = ()
  let on_exit ~id:_ ~duration_ns:_ = ()
  let on_event ~span:_ ~target:_ ~level:_ ~msg:_ ~fields:_ = ()
end

let worker @ portable = fun domain ->
  let level_name = Delator.Level.to_string Delator.Info in
  let enabled =
    Delator.Runtime.is_enabled ~level:Delator.Info ~target:"portable"
  in
  if enabled then
    Delator.Runtime.event ~target:"portable" ~level:Delator.Info
      ~msg:"direct" ~fields:[ "domain", Delator.Field.int domain ];
  [%log.info "extension" ~domain:(Delator.Field.int domain)];
  Delator.in_span ~level:Delator.Info ~target:"portable" ~name:level_name
    ~fields:(fun () -> [ "domain", Delator.Field.int domain ])
    (fun () -> Delator.Clock.now_ns ())
[@@delator.instrument] [@@delator.level info]

let () =
  Delator.Renderer.set_current (module Null_renderer);
  Delator.Clock.disable ();
  Delator.set_default_level Delator.Trace;
  let child =
    (Domain.Safe.spawn [@alert "-do_not_spawn_domains"]) (fun () -> worker 1)
  in
  let parent_result = worker 0 in
  let child_result = Domain.join child in
  assert (Int64.equal parent_result 0L);
  assert (Int64.equal child_result 0L)
