let run () =
  let[@log_value.trace] metadata = 7 in
  Suffix_provider.consume ~metadata:(metadata [@log_value.trace]) ()

module Suffix_provider = struct
  let consume ~metadata:(_metadata : int [@log_value.trace]) () = ()
end
