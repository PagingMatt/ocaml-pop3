(** Maildrop message parsing. *)

(** Module type for handling messages in the maildrop *)
module type MessageParser = sig
  (** Parses a message in the maildrop and returns an option of its lines. *)
  val lines_of_string : string -> string list option
end

(** The [JsonMessageParser] is an implementation of the [MessageParser]
    signature for messages stored in JSON format. *)
module JsonMessageParser : MessageParser
