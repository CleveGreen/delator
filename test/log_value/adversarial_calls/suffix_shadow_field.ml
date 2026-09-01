module N = struct
  module Suffix_provider = struct
    type record_payload = {
      stable : int;
      metadata : int [@log_value.trace];
    }
  end
end

let[@log_value.trace] make () =
  let[@log_value.trace] metadata = 7 in
  {
    Suffix_provider.stable = 0;
    Suffix_provider.metadata = (metadata [@log_value.trace]);
  }
