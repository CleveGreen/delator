let iterations =
  match Sys.getenv_opt "DELATOR_BENCH_ITERS" with
  | Some value -> int_of_string value
  | None -> 100_000_000

let consumed = ref 0
let consume value = consumed := Sys.opaque_identity value

let baseline value () = consume value

let elided value () =
  consume value;
  [%log.debug "statically removed" ~value:(Delator.Field.int value)]

let elapsed operation =
  let started_at = Unix.gettimeofday () in
  for _ = 1 to iterations do operation () done;
  (Unix.gettimeofday () -. started_at) *. 1e9 /. float iterations

let median values =
  let values = Array.of_list values in
  Array.sort Float.compare values;
  values.(Array.length values / 2)

let () =
  let baseline_operation = baseline 41 in
  let elided_operation = elided 41 in
  for _ = 1 to iterations / 20 do
    baseline_operation ();
    elided_operation ()
  done;
  let samples =
    List.init 9 (fun index ->
        if index mod 2 = 0 then
          (elapsed baseline_operation, elapsed elided_operation)
        else
          let elided = elapsed elided_operation in
          let baseline = elapsed baseline_operation in
          (baseline, elided))
  in
  let baseline = median (List.map fst samples) in
  let elided = median (List.map snd samples) in
  Printf.printf
    "static_elision\titerations=%d\tbaseline_ns/op=%.3f\telided_ns/op=%.3f\tdelta_ns/op=%.3f\n"
    iterations baseline elided (elided -. baseline)
