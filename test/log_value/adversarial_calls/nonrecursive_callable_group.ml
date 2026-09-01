module M = struct
  let consume value = value

  let consume (_metadata [@log_value.trace]) () = ()
  and alias = consume
end

let result = M.alias 7
