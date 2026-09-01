module N = struct
  module Suffix_provider = struct
    let[@log_value.trace] value = 7
  end
end

let[@log_value.trace] run () =
  let[@log_value.trace] copy =
    (Suffix_provider.value [@log_value.trace])
  in
  ignore (Sys.opaque_identity (copy [@log_value.trace]))
