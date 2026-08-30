let () =
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast
    "delator-composition-smoke" ~impl:Fun.id ~intf:Fun.id
