let () =
  match Delator.Renderer.json with
  | Some _ -> ()
  | None ->
      (Unix.putenv [@alert "-unsafe_multidomain"]) "DELATOR_FORMAT" "json";
      (match Delator.Renderer.configure_from_env () with
      | () -> failwith "expected unavailable JSON renderer"
      | exception Invalid_argument message ->
          assert (String.starts_with ~prefix:"DELATOR_FORMAT=json requires" message))
