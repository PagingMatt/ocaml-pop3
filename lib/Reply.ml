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
  val greeting : string -> Unix.tm -> t

  val internal_error : t
end = struct
  let greeting h t =
    let ft,_ = Unix.mktime t in
    (Printf.sprintf "<%f@%s>" ft h)
    |> Printf.sprintf "server ready %s"
    |> fun msg -> Ok ((Some msg), [])

  let internal_error = Error None
end

let ok fl ls = Ok (fl, ls)

let err fl = Error fl