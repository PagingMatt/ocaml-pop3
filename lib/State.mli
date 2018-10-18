(** State machine representing POP3 server. *)

open Unix

(** The inner state of the POP3 'Authorization' state.

    The [string option] value is the mailbox identifier if USER+PASS
    authorization is being used and the previously issued command was a
    successful USER command. *)
type authorization_state =
  | Banner of tm
  (** Initial state and that returned to on unsuccessful authorization attempts.

      The [tm] value is the 'banner time' of the connection which can be used
      for APOP authorization. *)
  | Mailbox of tm * string
  (** Intermediate state after successful USER command.

      The [tm] value is the 'banner time' of the connection which can be used
      for APOP authorization should the subsequent PASS command fail and the
      initial state is returned to. *)
  | Quit
  (** Abort state when QUIT command is issued. *)

(** The inner state of the POP3 'Transaction' state.

    The [string] value is the mailbox identifier that has been authorized
    previously in the session. *)
type transaction_state = string

(** The inner state of the POP3 'Update' state.

    The [string] value is the mailbox identifier that has been authorized
    previously in the session. *)
type update_state = string

type 'a command_result = {
  state : 'a;
  reply : Reply.t;
  next : bool;
}

(** Module type for handling commands while in the Authorization state. *)
module type Authorizer = sig
  val authorize :
    authorization_state -> Command.t
      -> (authorization_state command_result) Lwt.t
end

(** Module type for handling commands while in the Transaction state. *)
module type Transactor = sig
  val transact :
    transaction_state -> Command.t
      -> (transaction_state command_result) Lwt.t
end

(** Module type for handling commands while in the Update state. *)
module type Updater = sig
  val update :
    update_state -> Command.t
      -> (update_state command_result) Lwt.t
end

(** State functor encapsulates server POP3 server state. Its parameters handle
    commands for each state it can transition through. *)
module State (A : Authorizer) (T : Transactor) (U : Updater) : sig
  (** POP3 session states as defined in RFC 1939. *)
  type t =
    | Disconnected
    (** [Disconnected] represents the termination of the POP3 session. *)
    | Authorization of authorization_state
    (** [Authorization] represents the initial state of the POP3 session. *)
    | Transaction of transaction_state
    (** [Authorization] represents the intermediate state of the POP3
        session. *)
    | Update of update_state
    (** [Update] represents the final state of the POP3 session. *)

  (** Start a new POP3 session with 'banner time' of the [gmtime] whenever
      [start] is evaluated.

    @return a new state machine [t] in the initial [Authorization] state with a
            current 'banner time'. *)
  val start : unit -> t

  (** Function to drive state machine from the client command passed as an
    argument.

    TODO:
      1. Also return server response. *)
  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end
