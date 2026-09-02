let iterations_from_env default =
  match Sys.getenv_opt "DELATOR_BENCH_ITERS" with
  | Some value -> int_of_string value
  | None -> default

let allocated_words before after =
  (after.Gc.minor_words +. after.major_words -. after.promoted_words)
  -. (before.Gc.minor_words +. before.major_words -. before.promoted_words)

let median values =
  let values = Array.of_list values in
  Array.sort Float.compare values;
  values.(Array.length values / 2)

let measure ~iterations operation =
  for _ = 1 to max 1 (iterations / 20) do
    operation ()
  done;
  let samples =
    List.init 7 (fun _ ->
        Gc.full_major ();
        let allocation_before = Gc.quick_stat () in
        let started_at = Unix.gettimeofday () in
        for _ = 1 to iterations do
          operation ()
        done;
        let elapsed = Unix.gettimeofday () -. started_at in
        let allocation_after = Gc.quick_stat () in
        (elapsed *. 1e9 /. float iterations,
         allocated_words allocation_before allocation_after /. float iterations))
  in
  (median (List.map fst samples), median (List.map snd samples))

let report name iterations operation =
  let ns_per_op, words_per_op = measure ~iterations operation in
  Printf.printf "%s\titerations=%d\tns/op=%.3f\twords/op=%.5f\n%!"
    name iterations ns_per_op words_per_op

let consumed = ref 0
let consume value = consumed := Sys.opaque_identity value
let consumed_time = ref 0L
let consume_time value = consumed_time := Sys.opaque_identity value

module Null_renderer : Delator.Renderer.S = struct
  let on_new_span ~id:_ ~parent:_ ~name:_ ~target:_ ~level:_ ~fields:_ = ()
  let on_exit ~id:_ ~duration_ns:_ = ()
  let on_event ~span:_ ~target:_ ~level:_ ~msg:_ ~fields:_ = ()
end

let baseline value () = consume value

let check_disabled target () =
  consume
    (if Delator.Runtime.is_enabled ~level:Debug ~target then 1 else 0)

let ppx_event value () =
  [%log.debug "benchmark event" ~value:(Delator.Field.int value)]

let instrumented value = value + 1
[@@delator.instrument]

let instrumented_call value () = consume (instrumented value)

let instrumented_inline value = value + 1
[@@delator.instrument] [@@inline always]

let instrumented_inline_call value () = consume (instrumented_inline value)

let direct_event () =
  Delator.Runtime.event ~target:"bench" ~level:Debug ~msg:"benchmark event"
    ~fields:[]

let field_event () =
  Delator.Runtime.event ~target:"bench" ~level:Debug ~msg:"benchmark event"
    ~fields:
      [ ("name", Delator.Field.string "value");
        ("count", Delator.Field.int 41);
        ("cached", Delator.Field.bool true) ]

let direct_span value () =
  consume
    (Delator.in_span ~level:Debug ~target:"bench" ~name:"span"
       (fun () -> value + 1))

let clock_now () = consume_time (Delator.Clock.now_ns ())

let tree_exit =
  let module Tree = (val Delator.Renderer.tree) in
  fun () -> Tree.on_exit ~id:min_int ~duration_ns:Int64.min_int

let run name =
  match name with
  | "baseline" -> report name (iterations_from_env 50_000_000) (baseline 41)
  | "is_enabled" ->
      Delator.set_default_level Info;
      report name (iterations_from_env 50_000_000) (check_disabled "bench")
  | "is_enabled_directive" ->
      report name (iterations_from_env 30_000_000)
        (check_disabled "lowering.specialized")
  | "ppx_disabled" ->
      Delator.set_default_level Info;
      report name (iterations_from_env 30_000_000) (ppx_event 41)
  | "event_enabled_null" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      report name (iterations_from_env 20_000_000) direct_event
  | "ppx_enabled_null" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      report name (iterations_from_env 5_000_000) (ppx_event 41)
  | "instrument_disabled" ->
      Delator.set_default_level Info;
      report name (iterations_from_env 20_000_000) (instrumented_call 41)
  | "instrument_inline_disabled" ->
      Delator.set_default_level Info;
      report name (iterations_from_env 20_000_000) (instrumented_inline_call 41)
  | "instrument_enabled_null" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      Delator.Clock.disable ();
      report name (iterations_from_env 2_000_000) (instrumented_call 41)
  | "span_disabled" ->
      Delator.set_default_level Info;
      report name (iterations_from_env 20_000_000) (direct_span 41)
  | "span_enabled_null" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      Delator.Clock.disable ();
      report name (iterations_from_env 2_000_000) (direct_span 41)
  | "clock_monotonic" ->
      Delator.Clock.use_monotonic ();
      report name (iterations_from_env 10_000_000) clock_now
  | "clock_tsc" ->
      Delator.Clock.use_tsc ();
      report name (iterations_from_env 10_000_000) clock_now
  | "span_enabled_monotonic" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      Delator.Clock.use_monotonic ();
      report name (iterations_from_env 2_000_000) (direct_span 41)
  | "span_enabled_tsc" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      Delator.Clock.use_tsc ();
      report name (iterations_from_env 2_000_000) (direct_span 41)
  | "tree_event" ->
      Delator.set_default_level Trace;
      report name (iterations_from_env 500_000) direct_event
  | "tree_event_fields" ->
      Delator.set_default_level Trace;
      report name (iterations_from_env 500_000) field_event
  | "event_fields_null" ->
      Delator.set_default_level Trace;
      Delator.Renderer.set_current (module Null_renderer);
      report name (iterations_from_env 20_000_000) field_event
  | "tree_exit" ->
      report name (iterations_from_env 500_000) tree_exit
  | _ -> invalid_arg ("unknown benchmark: " ^ name)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline
      "usage: hot_paths.exe {baseline|is_enabled|is_enabled_directive|ppx_disabled|event_enabled_null|ppx_enabled_null|event_fields_null|instrument_disabled|instrument_inline_disabled|instrument_enabled_null|span_disabled|span_enabled_null|clock_monotonic|clock_tsc|span_enabled_monotonic|span_enabled_tsc|tree_event|tree_event_fields|tree_exit}";
    exit 2
  end;
  run Sys.argv.(1)
