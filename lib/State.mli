(** State machine representing POP3 server. *)

open Store
open Unix

(** Module type for generating 'banner time' when connections are created. *)
module type Banner = sig
  val time : unit -> tm
end

(** Implementation of [Banner] module type for [gmtime]. *)
module GmTimeBanner : Banner

(** The [State] signature encapsulates the types and values to construct the
    POP3 state machine. *)
module type State = sig
  (** POP3 session states as defined in RFC 1939. *)
  type pop3_session_state =
    | Disconnected
    (** [Disconnected] represents the termination of the POP3 session. *)
    | Authorization of string option
    (** [Authorization] represents the initial state of the POP3 session. *)
    | Transaction of string
    (** [Authorization] represents the intermediate state of the POP3
        session. *)
    | Update of string
    (** [Update] represents the final state of the POP3 session. *)

  (** The overall server state is a tuple of POP3 session state and the banner
      time of the connection. *)
  type t = pop3_session_state * tm

  (** Create a new POP3 session state machine.

    @return a new state machine [t] in the initial [Authorization] state with a
            current 'banner time'. *)
  val start : unit -> t

  (** Function to drive state machine from the client command passed as an
      argument.

    @return promise of a tuple of next state and reply to send back to client. *)
  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

(** The [BackingStoreState] module is an implementation of the [State] signature
    which is a functor over some [Banner] module and a backing [Store] module. *)
module BackingStoreState (B : Banner) (S : Store) : State
