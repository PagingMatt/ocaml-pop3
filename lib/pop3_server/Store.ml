module type Store = sig
  type t

  val init : string -> t Lwt.t

  val secret_of_mailbox : t -> string -> string option Lwt.t

  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t

  val message_list_of_mailbox : t -> string -> int list Lwt.t

  val octets_of_message : t -> string -> int -> int option Lwt.t

  val lines_of_message : t -> string -> int -> string list option Lwt.t

  val uid_of_message : t -> string -> int -> string option Lwt.t
end
