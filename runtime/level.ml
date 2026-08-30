type t = Trace | Debug | Info | Warn | Error

let[@inline always] to_int = function
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let compare left right = Int.compare (to_int left) (to_int right)

let of_string value =
  match String.lowercase_ascii (String.trim value) with
  | "trace" -> Ok Trace
  | "debug" -> Ok Debug
  | "info" -> Ok Info
  | "warn" | "warning" -> Ok Warn
  | "error" -> Ok Error
  | value -> Error (Printf.sprintf "unknown log level %S" value)

let to_string = function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
