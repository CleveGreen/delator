let levels = [ "trace"; "debug"; "info"; "warn"; "error" ]

let rank = function
  | "trace" -> 0
  | "debug" -> 1
  | "info" -> 2
  | "warn" -> 3
  | "error" -> 4
  | level -> invalid_arg level

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel contents)

let find_program name =
  let paths =
    Option.value ~default:"" (Sys.getenv_opt "PATH")
    |> String.split_on_char ':'
  in
  match
    List.find_map
      (fun directory ->
        let path = Filename.concat directory name in
        if Sys.file_exists path then Some path else None)
      paths
  with
  | Some path -> path
  | None -> failwith (name ^ " is not on PATH")

let environment level =
  let prefix = "DELATOR_TEST_STATIC_LEVEL=" in
  let inherited =
    Unix.environment ()
    |> Array.to_list
    |> List.filter (fun entry ->
           not
             (String.length entry >= String.length prefix
             && String.sub entry 0 (String.length prefix) = prefix))
  in
  Array.of_list ((prefix ^ level) :: inherited)

let run ~output ?level program arguments =
  let descriptor =
    Unix.openfile output [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let argv = Array.of_list (program :: arguments) in
  let environment =
    match level with Some level -> environment level | None -> Unix.environment ()
  in
  let pid =
    Unix.create_process_env program argv environment Unix.stdin descriptor descriptor
  in
  Unix.close descriptor;
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED status -> status
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let with_directory action =
  let directory = Filename.temp_file "delator-log-value" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir directory
      |> Array.iter (fun name -> Sys.remove (Filename.concat directory name));
      Unix.rmdir directory)
    (fun () -> action directory)

let check_lattice ~ocamlc ~ppx directory =
  let checked = ref 0 in
  List.iter
    (fun declared ->
      List.iter
        (fun context ->
          let stem = declared ^ "-" ^ context in
          let source = Filename.concat directory (stem ^ ".ml") in
          let output = Filename.concat directory (stem ^ ".output") in
          write source
            (Printf.sprintf
               "let run input =\n\
               \  let[@log_value.%s] value = input in\n\
               \  let[@log_value.%s] _sink =\n\
               \    (value [@log_value.%s])\n\
               \  in\n\
               \  ()\n"
               declared context context);
          let status =
            run ~output ~level:"trace" ocamlc
              [ "-stop-after"; "typing"; "-ppx"; ppx ^ " --as-ppx"; source ]
          in
          let accepted = status = 0 in
          let expected = rank context <= rank declared in
          if accepted <> expected then
            failwith
              (Printf.sprintf "unexpected %s -> %s result:\n%s" declared
                 context (read output));
          incr checked)
        levels)
    levels;
  Printf.printf "lattice=%d\n" !checked

let threshold_source =
  "let effects = ref 0\n\n\
   let mark () =\n\
   \  incr effects;\n\
   \  !effects\n\n\
   let run () =\n\
   \  let[@log_value.trace] trace = mark () in\n\
   \  let[@log_value.trace] _trace_use = (trace [@log_value.trace]) in\n\
   \  let[@log_value.debug] debug = mark () in\n\
   \  let[@log_value.debug] _debug_use = (debug [@log_value.debug]) in\n\
   \  let[@log_value.info] info = mark () in\n\
   \  let[@log_value.info] _info_use = (info [@log_value.info]) in\n\
   \  let[@log_value.warn] warn = mark () in\n\
   \  let[@log_value.warn] _warn_use = (warn [@log_value.warn]) in\n\
   \  let[@log_value.error] error = mark () in\n\
   \  let[@log_value.error] _error_use = (error [@log_value.error]) in\n\
   \  Printf.printf \"%d\\n\" !effects\n\n\
   let () = run ()\n"

let check_thresholds ~ocamlc ~ppx directory =
  let source = Filename.concat directory "thresholds.ml" in
  write source threshold_source;
  List.iter
    (fun level ->
      let executable = Filename.concat directory (level ^ ".exe") in
      let compile_output = Filename.concat directory (level ^ ".compile") in
      let status =
        run ~output:compile_output ~level ocamlc
          [ "-ppx"; ppx ^ " --as-ppx"; "-o"; executable; source ]
      in
      if status <> 0 then failwith (read compile_output);
      let run_output = Filename.concat directory (level ^ ".run") in
      let status = run ~output:run_output executable [] in
      if status <> 0 then failwith (read run_output);
      Printf.printf "%s=%s" level (read run_output))
    levels

let () =
  if Array.length Sys.argv <> 2 then
    invalid_arg "matrix_checks.exe PPX_DRIVER";
  let ocamlc = find_program "ocamlc" in
  let ppx = Sys.argv.(1) in
  with_directory (fun directory ->
      check_lattice ~ocamlc ~ppx directory;
      check_thresholds ~ocamlc ~ppx directory)
