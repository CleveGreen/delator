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

let color_enabled = ref false

let is_color_enabled () = !color_enabled
let set_color_enabled value = color_enabled := value

let terminal_supports_color () =
  Sys.getenv_opt "NO_COLOR" = None
  && not (Option.equal String.equal (Sys.getenv_opt "TERM") (Some "dumb"))
  && Unix.isatty (Unix.descr_of_out_channel stderr)

let configure_color_from_env () =
  match Sys.getenv_opt "DELATOR_COLOR" with
  | None | Some "" | Some "auto" -> set_color_enabled (terminal_supports_color ())
  | Some "always" | Some "1" -> set_color_enabled true
  | Some "never" | Some "0" -> set_color_enabled false
  | Some value ->
      invalid_arg
        (Printf.sprintf
           "DELATOR_COLOR: expected auto, always, or never, got %S" value)

let color_style = function
  | Trace -> "\027[2m"
  | Debug -> "\027[36m"
  | Info -> "\027[32m"
  | Warn -> "\027[33m"
  | Error -> "\027[1;31m"

let add_styled output style text =
  Stdlib.Buffer.add_string output style;
  Stdlib.Buffer.add_string output text;
  Stdlib.Buffer.add_string output "\027[0m"

let add_to_buffer output level =
  if !color_enabled then add_styled output (color_style level) (to_string level)
  else Stdlib.Buffer.add_string output (to_string level)

let add_span_marker output marker =
  if !color_enabled then add_styled output "\027[35m" marker
  else Stdlib.Buffer.add_string output marker
