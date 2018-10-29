module type MessageParser = sig
  val lines_of_string : string -> string list option

  val uid_of_string : string -> string option
end

module JsonMessageParser : MessageParser = struct
  let lines_of_string _msg =
    None

  let uid_of_string _msg =
    None
end
