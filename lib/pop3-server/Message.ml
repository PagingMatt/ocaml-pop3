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
      | [] -> acc
      | _  -> None

  let lines_of_string msg =
    match (from_string msg) with
    | `List lines ->
      (match fold_lines (Some []) lines with
      | Some ls -> Some (List.rev ls)
      | None    -> None)
    | _ -> None

  let uid_of_string msg =
    match (lines_of_string msg) with
    | Some ls -> Some
      (String.concat "\r\n" ls
      |> Digest.string
      |> Digest.to_hex)
    | None -> None
end
