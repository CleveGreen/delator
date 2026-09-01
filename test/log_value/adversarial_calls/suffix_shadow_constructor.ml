module N = struct
  module Suffix_provider = struct
    type variant_payload = Payload of int * (int [@log_value.trace])
  end
end

let[@log_value.trace] make () =
  let[@log_value.trace] metadata = 7 in
  Suffix_provider.Payload (1, (metadata [@log_value.trace]))
