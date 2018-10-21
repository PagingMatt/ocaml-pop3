type t =
  | Ok of string option * (string list)
  | Error of string option

let string_of_ok = "+OK"

let string_of_error = "-ERR"

let build_first_line indicator msg =
  if msg = "" then indicator else
  String.concat " " [indicator; msg]

let lines_of_t reply =
  match reply with
  | Error None           -> [string_of_error]
  | Error (Some msg)     -> [build_first_line string_of_error msg]
  | Ok (None, lines)     -> string_of_ok::lines
  | Ok (Some msg, lines) -> (build_first_line string_of_ok msg)::lines

module Common : sig
  val greeting : t

  val internal_error : t
end = struct
  let greeting = Ok ((Some "greetings from OCaml POP3"), [])

  let internal_error = Error None
end

let ok fl ls = Ok (fl, ls)

let err fl = Error fl