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
  (** The overall server state. *)
  type t

  (** Create a new POP3 session state machine.

    @return a new state machine [t] in the initial [Authorization] state with a
            current 'banner time'. The [string] parameter is used to for
            initializing the maildrop. *)
  val start : string -> t Lwt.t

  (** Function to drive state machine from the client command passed as an
      argument.

    @return promise of a tuple of next state and reply to send back to client. *)
  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

(** The [BackingStoreState] module is an implementation of the [State] signature
    which is a functor over some [Banner] module and a backing [Store] module. *)
module BackingStoreState (B : Banner) (S : Store) : State
