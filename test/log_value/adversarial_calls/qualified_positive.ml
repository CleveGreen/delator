(* Expected: accept.  This is the neighboring qualified call with matching
   metadata; it should remain valid at every profile retaining trace data. *)
module M = struct
  let consume ~metadata:(_metadata : int [@log_value.trace]) () = ()
end

let run () =
  let[@log_value.trace] metadata = 7 in
  M.consume ~metadata:(metadata [@log_value.trace]) ()

let () = run ()
