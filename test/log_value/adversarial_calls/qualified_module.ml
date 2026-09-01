(* Expected: reject before erasure: [M.consume]'s formal is ordinary, so the
   qualified call cannot authenticate a gated actual. *)
module M = struct
  let consume ~metadata:_metadata () = ()
end

let run () =
  let[@log_value.trace] metadata = 7 in
  M.consume ~metadata:(metadata [@log_value.trace]) ()
