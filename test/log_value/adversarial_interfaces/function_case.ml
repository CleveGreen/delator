let classify_annots ~source_file = function
  | 0 -> source_file
  | annotation -> source_file ^ string_of_int annotation
