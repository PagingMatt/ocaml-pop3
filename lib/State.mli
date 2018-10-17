(** State machine representing POP3 server. *)

open Unix

(** The inner state of the POP3 [Authorization] state. When the [Authorization]
    state is initially entered, [None] will be used, [Mailbox] is for when the
    USER command is used and successfully identifies a mailbox. *)
type authorization_state =
  | None
  (** No mailbox is being authenticated for via the USER/PASS commands. *)
  | Mailbox of string
  (** A USER command identified a valid mailbox, waiting on a PASS command. *)

(** POP3 session states as defined in RFC 1939. *)
type t =
  | Authorization of tm * authorization_state
  (** [Authorization] wraps around the banner time of the connection being
      opened (used for APOP authorization) and an incrementing authorization
      state which after a successful USER command will hold a mailbox
      identifier. *)
  | Transaction of string
  (** [Transaction] wraps around a mailbox identifier. *)
  | Update of string
  (** [Update] wraps around a mailbox identifier. *)
