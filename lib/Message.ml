module type MessageParser = sig
  val lines_of_string : string -> string list option
end

module JsonMessageParser : MessageParser = struct
  let lines_of_string _msg =
    None
end
