let register name =
  Ppxlib.Driver.register_transformation name
    ~impl:Delator_ppx_rewriter_selected.impl
    ~intf:Delator_ppx_rewriter_selected.intf

let () = register "delator"

let registered = ()

let configure_static_level_for_test =
  Delator_ppx_rewriter_selected.set_static_level

let register_for_test () = register "delator-dsource-test"
