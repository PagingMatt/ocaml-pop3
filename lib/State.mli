(** State machine representing POP3 server. *)

open Store
open Unix

(** Module type for generating 'banner time' when connections are created. *)
module type Banner = sig
  val time : unit -> tm
end

(** Implementation of [Banner] module type for [gmtime]. *)
module GmTimeBanner : Banner

(** State functor encapsulates server POP3 server state. Its parameters handle
    the greeting banner for new connections and the underlying store for secrets
    and mail drops. *)
module State (B : Banner) (S : Store) : sig
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

  type t = pop3_session_state * tm

  (** Start a new POP3 session with 'banner time' from [B] whenever [start] is
      evaluated.

    @return a new state machine [t] in the initial [Authorization] state with a
            current 'banner time'. *)
  val start : unit -> t

  (** Function to drive state machine from the client command passed as an
    argument.

    @return tuple of next state and reply to send to client in a lightweight
            thread. *)
  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end
