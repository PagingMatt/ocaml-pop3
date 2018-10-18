type t =
  | Ok of string option * (string list)
  | Error of string option

let string_of_ok = "+OK"

let string_of_error = "-ERR"

let build_first_line indicator msg =
  if msg = "" then indicator else
  String.concat " " [indicator; msg]

let string_of_t reply =
  match reply with
  | Error None           -> string_of_error
  | Error (Some msg)     -> build_first_line string_of_error msg
  | Ok (None, lines)     -> String.concat "\r\n" (string_of_ok::lines)
  | Ok (Some msg, lines) ->
    String.concat "\r\n" ((build_first_line string_of_ok msg)::lines)

let internal_error = Error None
