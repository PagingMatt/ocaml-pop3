module type MessageParser = sig
  val lines_of_string : string -> string list option

  val uid_of_string : string -> string option
end

module JsonMessageParser : MessageParser = struct
  open Yojson.Basic

  let rec fold_lines acc ls =
    match acc with
    | None       -> None
    | Some lines ->
      match ls with
      | (`String line)::ls' ->
        fold_lines (Some (line::lines)) ls'
      | _ -> None

  let lines_of_string msg =
    match (from_string msg) with
    | `List lines ->
      (* Top-level JSON structure should be a list. *)
      (match fold_lines (Some []) lines with
      | Some ls -> Some (List.rev ls)
      | None    -> None)
    | _ ->
      (* Any other top-level JSON structure is a failure. *)
      None

  let uid_of_string _msg =
    None
end
