module Trace = Trace_core

(* This benchmark state is created per domain and is never shared. OxCaml's
   portable DLS cannot express that ownership, so the conservative alert does
   not identify a race here. *)
[@@@alert "-unsafe_multidomain"]

type Trace.span += Null_span | Measured_span of int * int64

type measurement = { ns_per_op : float; words_per_op : float }

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

let sample ~iterations operation =
  Gc.full_major ();
  let allocation_before = Gc.quick_stat () in
  let started_at = Unix.gettimeofday () in
  for _ = 1 to iterations do
    operation ()
  done;
  let elapsed = Unix.gettimeofday () -. started_at in
  let allocation_after = Gc.quick_stat () in
  { ns_per_op = elapsed *. 1e9 /. float iterations;
    words_per_op =
      allocated_words allocation_before allocation_after /. float iterations }

let rotate count values =
  let rec split_at count prefix values =
    if count = 0 then (List.rev prefix, values)
    else
      match values with
      | value :: rest -> split_at (count - 1) (value :: prefix) rest
      | [] -> (List.rev prefix, [])
  in
  let prefix, suffix = split_at count [] values in
  suffix @ prefix

let report_group workload iterations operations =
  List.iter
    (fun (_, operation) ->
      for _ = 1 to max 1 (iterations / 20) do
        operation ()
      done)
    operations;
  let samples = Hashtbl.create (List.length operations) in
  List.iter (fun (name, _) -> Hashtbl.add samples name []) operations;
  for sample_index = 0 to 8 do
    List.iter
      (fun (name, operation) ->
        Hashtbl.replace samples name
          (sample ~iterations operation :: Hashtbl.find samples name))
      (rotate (sample_index mod (List.length operations)) operations)
  done;
  List.iter
    (fun (library, _) ->
      let library_samples = Hashtbl.find samples library in
      let result =
        { ns_per_op =
            median
              (List.map (fun sample -> sample.ns_per_op) library_samples);
          words_per_op =
            median
              (List.map (fun sample -> sample.words_per_op) library_samples) }
      in
      Printf.printf
        "%s\t%s\titerations=%d\tns/op=%.3f\twords/op=%.5f\n%!"
        workload library iterations result.ns_per_op result.words_per_op)
    operations

let consumed = ref 0
let consume value = consumed := Sys.opaque_identity value

module Null_renderer : Delator.Renderer.S = struct
  let on_new_span ~id:_ ~parent:_ ~name:_ ~target:_ ~level:_ ~fields:_ = ()
  let on_exit ~id:_ ~duration_ns:_ = ()
  let on_event ~span:_ ~target:_ ~level:_ ~msg:_ ~fields:_ = ()
end

let null_trace_collector =
  let callbacks =
    Trace.Collector.Callbacks.make
      ~enter_span:(fun () ~__FUNCTION__:_ ~__FILE__:_ ~__LINE__:_ ~level:_
                        ~params:_ ~data:_ ~parent:_ _ -> Null_span)
      ~exit_span:(fun () _ -> ())
      ~add_data_to_span:(fun () _ _ -> ())
      ~message:(fun () ~level:_ ~params:_ ~data:_ ~span:_ _ -> ())
      ~metric:(fun () ~level:_ ~params:_ ~data:_ _ _ -> ())
      ()
  in
  Trace.Collector.C_some ((), callbacks)

type trace_id_state = { mutable next_id : int; mutable id_limit : int }

let trace_id_block_size = 4_096
let next_trace_id_block = Atomic.make 1
let trace_ids =
  Domain.DLS.new_key (fun () -> { next_id = 0; id_limit = 0 })

let fresh_trace_id () =
  let state = Domain.DLS.get trace_ids in
  if state.next_id = state.id_limit then begin
    let first = Atomic.fetch_and_add next_trace_id_block trace_id_block_size in
    state.next_id <- first;
    state.id_limit <- first + trace_id_block_size
  end;
  let id = state.next_id in
  state.next_id <- id + 1;
  id

let consumed_duration = ref 0L

let measured_trace_collector =
  let callbacks =
    Trace.Collector.Callbacks.make
      ~enter_span:(fun () ~__FUNCTION__:_ ~__FILE__:_ ~__LINE__:_ ~level:_
                        ~params:_ ~data:_ ~parent:_ _ ->
        Measured_span (fresh_trace_id (), Delator.Clock.now_ns ()))
      ~exit_span:(fun () span ->
        match span with
        | Measured_span (_, started_at) ->
            consumed_duration :=
              Sys.opaque_identity
                (Int64.sub (Delator.Clock.now_ns ()) started_at)
        | _ -> assert false)
      ~add_data_to_span:(fun () _ _ -> ())
      ~message:(fun () ~level:_ ~params:_ ~data:_ ~span:_ _ -> ())
      ~metric:(fun () ~level:_ ~params:_ ~data:_ _ _ -> ())
      ()
  in
  Trace.Collector.C_some ((), callbacks)

let baseline value () = consume value

let delator_event value () =
  [%log.debug "benchmark event"];
  consume value

let trace_event value () =
  Trace.message ~level:Debug1 "benchmark event";
  consume value

let delator_event_field value () =
  [%log.debug "benchmark event" ~value:(Delator.Field.int value)];
  consume value

let trace_event_field value () =
  Trace.message ~level:Debug1
    ~data:(fun () -> [ "value", `Int value ])
    "benchmark event";
  consume value

let delator_span value () =
  consume
    (Delator.in_span ~level:Debug ~target:"bench" ~name:"benchmark span"
       (fun () -> value))

let trace_span value () =
  Trace.with_span ~level:Debug1 ~__FILE__ ~__LINE__ "benchmark span"
    (fun _ -> consume value)

let setup_enabled ~timed =
  Delator.set_default_level Trace;
  Delator.Renderer.set_current (module Null_renderer);
  if timed then Delator.Clock.use_monotonic () else Delator.Clock.disable ();
  Trace.set_current_level Trace;
  Trace.setup_collector
    (if timed then measured_trace_collector else null_trace_collector)

let run workload =
  let value = 41 in
  let enabled = String.starts_with ~prefix:"enabled_" workload in
  (* The public span comparison includes the work a useful collector needs.
     The null case is retained only as an explicitly named dispatch floor. *)
  let timed = workload = "enabled_span" in
  if enabled then setup_enabled ~timed else Delator.set_default_level Info;
  let iterations =
    iterations_from_env
      (match workload with
      | "disabled_event" | "disabled_event_field" -> 30_000_000
      | "disabled_span" -> 20_000_000
      | "enabled_event" -> 10_000_000
      | "enabled_event_field" -> 5_000_000
      | "enabled_span" | "enabled_span_null" -> 2_000_000
      | _ -> invalid_arg ("unknown workload: " ^ workload))
  in
  let compared_operations =
    match workload with
    | "disabled_event" | "enabled_event" ->
        [ "delator", delator_event value; "ocaml-trace", trace_event value ]
    | "disabled_event_field" | "enabled_event_field" ->
        [ "delator", delator_event_field value;
          "ocaml-trace", trace_event_field value ]
    | "disabled_span" | "enabled_span" | "enabled_span_null" ->
        [ "delator", delator_span value; "ocaml-trace", trace_span value ]
    | _ -> assert false
  in
  report_group workload iterations
    (("baseline", baseline value) :: compared_operations);
  if enabled then Trace.shutdown ()

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline
      "usage: compare_ocaml_trace.exe {disabled_event|disabled_event_field|disabled_span|enabled_event|enabled_event_field|enabled_span|enabled_span_null}";
    exit 2
  end;
  run Sys.argv.(1)
