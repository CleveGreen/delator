let () =
  Delator.init ();
  let module Tree = (val Delator.Renderer.tree) in
  Tree.on_exit ~id:0 ~duration_ns:0L;
  Tree.on_exit ~id:max_int ~duration_ns:Int64.max_int;
  Tree.on_exit ~id:min_int ~duration_ns:Int64.min_int
