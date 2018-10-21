(** Store of mailbox secrets and maildrop content. *)

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

      @return [None] or [Some "<{tm}@{hostname}>{secret}"] dependending on
              whether the secret store of [t] contains the secret for the
              mailbox.*)
  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t
end

(** The [IrminStore] module is an implementation of the [Store] signature which
    uses Irmin for the underlying store. *)
module IrminStore : Store
