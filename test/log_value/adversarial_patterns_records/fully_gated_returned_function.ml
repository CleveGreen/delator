(* Expected: reject.  Every formal of the returned nested function is gated;
   collapsing it must not move the observable calls across the function
   boundary. *)
let observe_trace () (_value : int [@log_value.trace]) = print_endline "trace"
let observe_debug () (_value : int [@log_value.debug]) = print_endline "debug"

let make () =
  fun (outer [@log_value.trace]) ->
    observe_trace () (outer [@log_value.trace]);
    fun (inner [@log_value.debug]) ->
      observe_debug () (inner [@log_value.debug])
