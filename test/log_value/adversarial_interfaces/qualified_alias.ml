module Diagnostics = struct
  let error_diagnostic message = "error: " ^ message
end

let error_diagnostic = Diagnostics.error_diagnostic
