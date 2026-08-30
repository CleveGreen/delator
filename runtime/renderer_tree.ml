let on_new_span ~id:_ ~parent:_ ~name ~target ~level ~fields =
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  Render_util.add_indent output (Span.depth ());
  Stdlib.Buffer.add_string output (Level.to_string level);
  Stdlib.Buffer.add_char output ' ';
  Stdlib.Buffer.add_string output target;
  Stdlib.Buffer.add_string output "::";
  Stdlib.Buffer.add_string output name;
  Render_util.add_fields output fields;
  Buffer.finish_line line

let on_enter ~id:_ = ()

let on_exit ~id ~duration_ns =
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  let scratch = Buffer.decimal_scratch line in
  Render_util.add_indent output (Span.depth ());
  Stdlib.Buffer.add_string output "span#";
  Render_util.add_int ~scratch output id;
  Stdlib.Buffer.add_string output " close duration_ns=";
  Render_util.add_int64 ~scratch output duration_ns;
  Buffer.finish_line line

let on_event ~span:_ ~target ~level ~msg ~fields =
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  Render_util.add_indent output (Span.depth ());
  Stdlib.Buffer.add_string output (Level.to_string level);
  Stdlib.Buffer.add_char output ' ';
  Stdlib.Buffer.add_string output target;
  Stdlib.Buffer.add_string output ": ";
  Stdlib.Buffer.add_string output msg;
  Render_util.add_fields output fields;
  Buffer.finish_line line
