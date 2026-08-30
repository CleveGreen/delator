(* This mutable renderer state is created and accessed only through the
   current domain's DLS slot. *)
[@@@alert "-unsafe_multidomain"]

type state = {
  names : (int, string) Hashtbl.t;
  mutable stack : (int * string) list;
}

let state = Domain.DLS.new_key (fun () -> { names = Hashtbl.create 16; stack = [] })

let add_breadcrumb output stack =
  let rec add = function
    | [] -> ()
    | [ (_, name) ] -> Stdlib.Buffer.add_string output name
    | (_, name) :: rest ->
        add rest;
        Stdlib.Buffer.add_char output '>';
        Stdlib.Buffer.add_string output name
  in
  add stack

let on_new_span ~id ~parent:_ ~name ~target ~level ~fields =
  let state = Domain.DLS.get state in
  Hashtbl.replace state.names id name;
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  let scratch = Buffer.decimal_scratch line in
  Level.add_to_buffer output level;
  Stdlib.Buffer.add_char output ' ';
  Stdlib.Buffer.add_string output target;
  Stdlib.Buffer.add_string output " span.new=";
  Stdlib.Buffer.add_string output name;
  Render_util.add_fields ~scratch output fields;
  Buffer.finish_line line

let on_enter ~id =
  let state = Domain.DLS.get state in
  let name =
    match Hashtbl.find_opt state.names id with
    | Some name -> name
    | None -> Printf.sprintf "span#%d" id
  in
  state.stack <- (id, name) :: state.stack

let on_exit ~id ~duration_ns =
  let state = Domain.DLS.get state in
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  let scratch = Buffer.decimal_scratch line in
  Level.add_span_marker output "SPAN";
  Stdlib.Buffer.add_char output ' ';
  add_breadcrumb output state.stack;
  Stdlib.Buffer.add_string output " close=";
  Render_util.add_int ~scratch output id;
  Stdlib.Buffer.add_string output " duration_ns=";
  Render_util.add_int64 ~scratch output duration_ns;
  Buffer.finish_line line;
  (match state.stack with
  | (current, _) :: rest when current = id -> state.stack <- rest
  | _ -> ());
  Hashtbl.remove state.names id

let on_event ~span:_ ~target ~level ~msg ~fields =
  let state = Domain.DLS.get state in
  let line = Buffer.line_buffer () in
  let output = Buffer.output line in
  let scratch = Buffer.decimal_scratch line in
  Level.add_to_buffer output level;
  Stdlib.Buffer.add_char output ' ';
  Stdlib.Buffer.add_string output target;
  (match state.stack with
  | [] -> ()
  | stack ->
      Stdlib.Buffer.add_string output " [";
      add_breadcrumb output stack;
      Stdlib.Buffer.add_char output ']');
  Stdlib.Buffer.add_string output ": ";
  Stdlib.Buffer.add_string output msg;
  Render_util.add_fields ~scratch output fields;
  Buffer.finish_line line
