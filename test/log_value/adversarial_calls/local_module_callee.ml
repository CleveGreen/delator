let run () =
  let module M = struct
    let consume (_metadata [@log_value.trace]) () = ()
  end in
  let[@log_value.trace] metadata = 1 in
  M.consume (metadata [@log_value.trace]) ()
