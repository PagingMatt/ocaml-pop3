open Store
open Unix

module type Banner = sig
  val time : unit -> tm
end

module GmTimeBanner : Banner = struct
  let time () =
    time () |> gmtime
end

module type State = sig
  type pop3_session_state =
    | Disconnected
    | Authorization of string option
    | Transaction   of string
    | Update        of string

  type t = pop3_session_state * tm

  val start : unit -> t

  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

module BackingStoreState (B : Banner) (S : Store) : State = struct
  type pop3_session_state =
    | Disconnected
    | Authorization of string option
    | Transaction   of string
    | Update        of string

  type t = pop3_session_state * tm

  let start () = (Authorization None, B.time ())

  let f (state, banner_time) _cmd =
    match state with
    | Disconnected ->
      Lwt.return ((Disconnected, banner_time), Reply.internal_error)
    | Authorization _ ->
      Lwt.return ((Disconnected, banner_time), Reply.internal_error)
    | Transaction _ ->
      Lwt.return ((Disconnected, banner_time), Reply.internal_error)
    | Update _ ->
      Lwt.return ((Disconnected, banner_time), Reply.internal_error)
end
