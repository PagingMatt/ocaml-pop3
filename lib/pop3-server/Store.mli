(** Store of mailbox secrets and maildrop content. *)

open Message

(** Module type for store of mailbox secrets and maildrop content. *)
module type Store = sig
  (** Store. *)
  type t

  (** Initialize a new store within a lightweight thread.

      The [string] parameter indicates the location of the maildrop. *)
  val init : string -> t Lwt.t

  (** Find the shared secret within [t] for the [string] corresponding to the
      [string] parameter if it exists in the secret store for [t].

      @return [None] or [Some secret] depending on whether the secret store of
              [t] contains the secret for the mailbox. This is wrapped in a
              lightweight thread. *)
  val secret_of_mailbox : t -> string -> string option Lwt.t

  (** Find the APOP digest for a given mailbox at a given banner time, on the
      given host using the secret from the secret store of [t].

      @return [None] or [Some "<{tm}@{hostname}>{secret}"] depending on
              whether the secret store of [t] contains the secret for the
              mailbox.*)
  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t

  (** Finds the message numbers of the messages in the mailbox.

      @return [None] if the mailbox does not exist in [t] or [Some ms] if the
              mailbox does exist, where [ms] is a list of message numbers. *)
  val message_list_of_mailbox : t -> string -> int list Lwt.t

  (** Finds the number of octets in a given message.

      @return [None] if message doesn't exist in mailbox in [t] or [Some n]
              where [n] is the number of octets in the message. *)
  val octets_of_message : t -> string -> int -> int option Lwt.t

  (** Read a message from store [t] in mailbox corresponding to the [string]
      parameter with id corresponding to the [int] parameter.

      @return [None] or [Some message_lines] depdending on whether the message
              exists in the mailbox. This is wrapped in a lightweight thread.*)
  val lines_of_message : t -> string -> int -> string list option Lwt.t

  (** Get the unique ID for a message from store [t] in mailbox corresponding
      to the [string] parameter with id corresponding to the [int] parameter.

      @return [None] or [Some uid] depending on whether the message
              exists in the mailbox. This is wrapped in a lightweight thread.*)
  val uid_of_message : t -> string -> int -> string option Lwt.t
end

module type IrminStringKv =
  Irmin.KV with type contents = Irmin.Contents.String.t

module IrminStore (S : IrminStringKv) (P : MessageParser) : Store
