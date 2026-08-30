type line = { output : Stdlib.Buffer.t; decimal_scratch : bytes }

let create_line capacity =
  { output = Stdlib.Buffer.create capacity; decimal_scratch = Bytes.create 21 }

let local = Domain.DLS.new_key (fun () -> create_line 4096)
let flush_lock = Mutex.create ()
let unbuffered = ref false
let hook_installed = ref false

let write_chunk chunk =
  Mutex.lock flush_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock flush_lock)
    (fun () -> output_string stderr chunk; flush stderr)

let line_buffer () =
  if !unbuffered then create_line 128 else Domain.DLS.get local

let output line = line.output
let decimal_scratch line = line.decimal_scratch

let finish_line line =
  Stdlib.Buffer.add_char line.output '\n';
  if !unbuffered then write_chunk (Stdlib.Buffer.contents line.output)

let flush () =
  let buffer = (Domain.DLS.get local).output in
  if Stdlib.Buffer.length buffer <> 0 then begin
    let chunk = Stdlib.Buffer.contents buffer in
    Stdlib.Buffer.clear buffer;
    write_chunk chunk
  end

let configure_from_env () =
  unbuffered := Option.equal String.equal (Sys.getenv_opt "DELATOR_UNBUFFERED") (Some "1")

let install_at_exit () =
  if not !hook_installed then begin
    hook_installed := true;
    at_exit flush
  end
