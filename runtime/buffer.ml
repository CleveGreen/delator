(* The mutable line is created and accessed only by its owning domain. Shared
   sink writes go through [flush_lock]; Safe.DLS cannot express that split. *)
[@@@alert "-unsafe_multidomain"]

type line = { output : Stdlib.Buffer.t; decimal_scratch : bytes }

let create_line capacity =
  { output = Stdlib.Buffer.create capacity; decimal_scratch = Bytes.create 21 }

let flush_lock = Mutex.create ()
let unbuffered = ref false
let hook_installed = ref false
let chunk_limit = 65_536

let write_chunk chunk =
  Mutex.lock flush_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock flush_lock)
    (fun () -> output_string stderr chunk; flush stderr)

let flush_line line =
  if Stdlib.Buffer.length line.output <> 0 then begin
    let chunk = Stdlib.Buffer.contents line.output in
    Stdlib.Buffer.reset line.output;
    write_chunk chunk
  end

let local =
  Domain.DLS.new_key (fun () ->
      let line = create_line 4096 in
      Domain.at_exit (fun () -> flush_line line);
      line)

let line_buffer () =
  if !unbuffered then create_line 128 else Domain.DLS.get local

let output line = line.output
let decimal_scratch line = line.decimal_scratch

let finish_line line =
  Stdlib.Buffer.add_char line.output '\n';
  if !unbuffered || Stdlib.Buffer.length line.output >= chunk_limit then
    flush_line line

let flush () = flush_line (Domain.DLS.get local)

let configure_from_env () =
  unbuffered := Option.equal String.equal (Sys.getenv_opt "DELATOR_UNBUFFERED") (Some "1")

let install_at_exit () =
  if not !hook_installed then begin
    hook_installed := true;
    at_exit flush
  end
