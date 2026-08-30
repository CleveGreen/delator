let () =
  let open Delator.Level in
  assert (compare Trace Debug < 0);
  assert (compare Debug Info < 0);
  assert (compare Info Warn < 0);
  assert (compare Warn Error < 0)
