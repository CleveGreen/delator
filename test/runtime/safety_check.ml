[@@@alert "-unsafe_multidomain"]
[@@@alert "-do_not_spawn_domains"]

let expect_invalid operation =
  match operation () with
  | () -> failwith "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

module Null_renderer : Delator.Renderer.S = struct
  let on_new_span ~id:_ ~parent:_ ~name:_ ~target:_ ~level:_ ~fields:_ = ()
  let on_exit ~id:_ ~duration_ns:_ = ()
  let on_event ~span:_ ~target:_ ~level:_ ~msg:_ ~fields:_ = ()
end

module Failing_renderer : Delator.Renderer.S = struct
  let on_new_span ~id:_ ~parent:_ ~name:_ ~target:_ ~level:_ ~fields:_ =
    failwith "renderer failed"
  let on_exit ~id:_ ~duration_ns:_ = ()
  let on_event ~span:_ ~target:_ ~level:_ ~msg:_ ~fields:_ = ()
end

let new_span name =
  Delator.Runtime.new_span ~target:"safety" ~level:Info ~name ~fields:[]

let () =
  Delator.Renderer.set_current (module Null_renderer);
  Delator.Clock.set (fun () -> 0L);
  let rec nested count =
    if count > 0 then
      Delator.in_span ~level:Info ~target:"safety" ~name:"nested"
        (fun () -> nested (count - 1))
  in
  nested 40;
  Delator.Renderer.set_current (module Failing_renderer);
  (match
     Delator.in_span ~level:Info ~target:"safety" ~name:"failure" (fun () -> ())
   with
  | () -> failwith "expected renderer failure"
  | exception Failure message when message = "renderer failed" -> ()
  | exception error -> raise error);
  Delator.Renderer.set_current (module Null_renderer);
  nested 1;
  Delator.Clock.disable ();
  assert (Delator.Clock.now_ns () = 0L);
  nested 40;
  Delator.Clock.set (fun () -> 0L);
  let span = new_span "single-use" in
  Delator.Runtime.enter span;
  Delator.Runtime.exit span;
  expect_invalid (fun () -> Delator.Runtime.enter span);
  expect_invalid (fun () -> Delator.Runtime.exit span);
  let outer = new_span "outer" in
  Delator.Runtime.enter outer;
  let inner = new_span "inner" in
  Delator.Runtime.enter inner;
  expect_invalid (fun () -> Delator.Runtime.exit outer);
  Delator.Runtime.exit inner;
  Delator.Runtime.exit outer;
  let foreign = new_span "foreign" in
  let child =
    Domain.spawn (fun () ->
        expect_invalid (fun () -> Delator.Runtime.enter foreign))
  in
  Domain.join child;
  let output = Stdlib.Buffer.create 8 in
  expect_invalid (fun () ->
      Delator.Field.add_to_buffer ~scratch:(Bytes.create 0) output
        (Delator.Field.int min_int));
  Delator.Clock.use_monotonic ();
  let before = Delator.Clock.now_ns () in
  let after = Delator.Clock.now_ns () in
  assert (Int64.compare before after <= 0);
  (try
     Delator.Clock.use_tsc ();
     let started_at = Delator.Clock.now_ns () in
     Unix.sleepf 0.005;
     let elapsed_ns = Int64.sub (Delator.Clock.now_ns ()) started_at in
     assert (Int64.compare elapsed_ns 1_000_000L >= 0);
     assert (Int64.compare elapsed_ns 5_000_000_000L < 0)
   with Invalid_argument _ -> ())
