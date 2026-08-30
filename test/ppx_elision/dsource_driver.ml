let () =
  ignore Delator_ppx.registered;
  Delator_ppx.configure_static_level_for_test
    (Option.value ~default:"info" (Sys.getenv_opt "DSOURCE_STATIC_LEVEL"));
  Delator_ppx.register_for_test ();
  Ppxlib.Driver.standalone ()
