let () =
  ignore Delator_ppx.registered;
  Delator_ppx.configure_static_level_for_test
    (Option.value ~default:"trace"
       (Sys.getenv_opt "DELATOR_TEST_STATIC_LEVEL"));
  Ppxlib.Driver.standalone ()
