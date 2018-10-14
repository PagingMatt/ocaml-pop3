type status_indicator =
  | Ok
  | Error

let string_of_status_indicator si =
  match si with
  | Ok    -> "+OK"
  | Error -> "-ERR"
  