open Lwt.Infix
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
  type t

  val start : string -> t Lwt.t

  val terminated : t -> bool

  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

module BackingStoreState (B : Banner) (S : Store) : State = struct
  open Command

  type pop3_session_state =
    | Disconnected
    | Authorization of string option
    | Transaction   of string
    (*| Update        of string*)

  type t = pop3_session_state * tm * S.t

  let start p =
    S.init p
    >|= fun store -> (Authorization None, B.time (), store)

  let terminated (s,_,_) = (s = Disconnected)

  let auth_fail store banner_time =
    ((Authorization None, banner_time, store), Reply.err None)

  let auth_result store banner_time mailbox success =
    if success then
      ((Transaction mailbox, banner_time, store), Reply.ok (Some mailbox) [])
    else auth_fail store banner_time

  let auth_apop store banner_time mailbox digest =
    S.apop_of_mailbox store banner_time "" (* TODO: Add hostname to state *) mailbox
    >|= fun digest_option ->
      match digest_option with
      | None -> auth_fail store banner_time
      | Some digest' -> auth_result store banner_time mailbox (digest = digest')

  let auth_pass store banner_time mailbox secret =
    S.secret_of_mailbox store mailbox
    >|= fun secret_option ->
      match secret_option with
      | None -> auth_fail store banner_time
      | Some secret' -> auth_result store banner_time mailbox (secret = secret')

  let auth_user store banner_time mailbox =
    Lwt.return
      ((Authorization (Some mailbox), banner_time, store),
        Reply.ok (Some mailbox) [])

  let f_auth_none store banner_time cmd =
    match cmd with
    | User mailbox -> auth_user store banner_time mailbox
    | Apop (mailbox, digest) -> auth_apop store banner_time mailbox digest
    | _ -> Lwt.return (auth_fail store banner_time)

  let f_auth_some store banner_time mailbox cmd =
    match cmd with
    | Pass secret -> auth_pass store banner_time mailbox secret
    | _ -> Lwt.return (auth_fail store banner_time)

  let f (state, banner_time, store) cmd =
    match state with
    | Authorization None -> f_auth_none store banner_time cmd
    | Authorization (Some mailbox) -> f_auth_some store banner_time mailbox cmd
    | Disconnected ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)
    | Transaction _ ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)
    (*| Update _ ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)*)
end
