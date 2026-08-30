let transformed_implementation = ref false

let implementation structure =
  transformed_implementation := true;
  structure

let () =
  at_exit (fun () ->
      if not !transformed_implementation then
        failwith "current-AST composition transformation did not run");
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast
    "delator-composition-smoke" ~impl:implementation ~intf:Fun.id
