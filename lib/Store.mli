module type Store = sig
  type t

  val secret_of_mailbox : t -> string -> string option Lwt.t

  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t
end
