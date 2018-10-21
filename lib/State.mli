(** POP3 session state machine. *)

open Store
open Unix

(** Module type for generating 'banner time' when connections are created. *)
module type Banner = sig
  val time : unit -> tm
end

(** Implementation of [Banner] module type for current [gmtime]. *)
module GmTimeBanner : Banner

(** Module type for POP3 session state machine. *)
module type State = sig
  (** POP3 session state. *)
  type t

  (** Create a new POP3 session state machine.

    @return a new state machine [t] in the initial [Authorization] state with a
            current 'banner time'. The first [string] parameter is used for the
            hostname and the second is for initializing the maildrop. *)
  val start : string -> string -> t Lwt.t

  (** Predicate to determine if session is terminated.

      @return [true] if the state [t] passed in would inidicate a terminated
              session. *)
  val terminated : t -> bool

  (** Function to drive state machine.

      Starting from some state [t] the [Command.t] is applied to the state to
      determine the subsequent state and the reply to send back to the client.

    @return promise of a tuple of next state and reply to send back to client. *)
  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

(** The [BackingStoreState] module is an implementation of the [State] signature
    which is a functor over some [Banner] module and a backing [Store] module.

    The [Banner] module this is applied to determines the banner time presented
    to the client at the start of a session.

    The [Store] module this is applied to abstracts secret management for the
    various mailboxes known to a session, as well as the actual backing store
    for the maildrop itself. *)
module BackingStoreState (B : Banner) (S : Store) : State
