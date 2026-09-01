type error = Backend_failure of string

module For_testing = struct
  type controlled_failure = Unknown | Backend_failure
end

let backend_failure exn = Error (Backend_failure (Printexc.to_string exn))
