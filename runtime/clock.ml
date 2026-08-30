let wall_clock () = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.)
let current = ref wall_clock
let now_ns () = (!current) ()
let set clock = current := clock

let configure_from_env () =
  match Sys.getenv_opt "DELATOR_TEST_CLOCK" with
  | Some "1" ->
      let value = ref 0L in
      set (fun () ->
          let result = !value in
          value := Int64.add result 1_000_000L;
          result)
  | _ -> ()
