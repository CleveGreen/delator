external monotonic_now_ns_ : int64 -> int64 =
  "delator_monotonic_now_ns" "delator_monotonic_now_ns_unboxed"
  [@@unboxed] [@@noalloc]

external tsc_supported : unit -> bool = "delator_tsc_supported" [@@noalloc]
external tsc_now_ : int64 -> int64 =
  "delator_tsc_now" "delator_tsc_now_unboxed" [@@unboxed] [@@noalloc]

external tsc_nanoseconds_per_tick : unit -> float = "delator_tsc_nanoseconds_per_tick"

let zero = 0L
let monotonic_now_ns () = monotonic_now_ns_ zero
let tsc_now () = tsc_now_ zero

type tsc_source = {
  nanoseconds_per_tick : float;
  origin_ticks : int64;
  origin_ns : int64;
}

type source = Disabled | Monotonic | Tsc of tsc_source | Custom of (unit -> int64)

type sample = int64

let zero_sample = 0L

let current = ref Monotonic

let[@inline always] is_enabled () =
  match !current with Disabled -> false | Monotonic | Tsc _ | Custom _ -> true

let tsc_ticks_to_ns source ticks =
  Int64.add source.origin_ns
    (Int64.of_float
       (Int64.to_float (Int64.sub ticks source.origin_ticks)
       *. source.nanoseconds_per_tick))

let now_ns () =
  match !current with
  | Disabled -> zero
  | Monotonic -> monotonic_now_ns ()
  | Tsc source -> tsc_ticks_to_ns source (tsc_now ())
  | Custom clock -> clock ()

let[@inline always] sample () =
  match !current with
  | Disabled -> zero_sample
  | Monotonic -> monotonic_now_ns ()
  | Tsc _ -> tsc_now ()
  | Custom clock -> clock ()

let[@inline always] elapsed_ns started_at =
  match !current with
  | Disabled -> zero
  | Monotonic -> Int64.sub (monotonic_now_ns ()) started_at
  | Tsc source ->
      Int64.of_float
        (Int64.to_float (Int64.sub (tsc_now ()) started_at)
        *. source.nanoseconds_per_tick)
  | Custom clock -> Int64.sub (clock ()) started_at

let set clock = current := Custom clock

let disable () = current := Disabled

let use_monotonic () = current := Monotonic

let use_tsc () =
  if not (tsc_supported ()) then
    invalid_arg "DELATOR_CLOCK=tsc requires an invariant TSC";
  let nanoseconds_per_tick = tsc_nanoseconds_per_tick () in
  if nanoseconds_per_tick <= 0. then
    invalid_arg "DELATOR_CLOCK=tsc could not calibrate the TSC";
  let origin_ticks = tsc_now () in
  let origin_ns = monotonic_now_ns () in
  current := Tsc { nanoseconds_per_tick; origin_ticks; origin_ns }

let configure_from_env () =
  match Sys.getenv_opt "DELATOR_TEST_CLOCK" with
  | Some "1" ->
      let value = ref 0L in
      set (fun () ->
          let result = !value in
          value := Int64.add result 1_000_000L;
          result)
  | _ -> (
      match Sys.getenv_opt "DELATOR_CLOCK" with
      | None | Some "" | Some "monotonic" -> use_monotonic ()
      | Some "off" -> disable ()
      | Some "tsc" -> use_tsc ()
      | Some value ->
          invalid_arg
            (Printf.sprintf
               "DELATOR_CLOCK: expected monotonic, tsc, or off, got %S" value))
