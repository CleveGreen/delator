type t = {
  stable : int;
  info : int [@log_value.info];
}

let make ~stable ~(info : int [@log_value.info]) =
  { stable; info = (info [@log_value.info]) }

let consume () (_info : int [@log_value.info]) = ()
let[@log_value.info] info_value = 17
let[@log_value.trace] trace_value = 23
