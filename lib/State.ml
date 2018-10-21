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

  val start : string -> string -> t Lwt.t

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

  type t = string * pop3_session_state * tm * S.t

  let start h p =
    S.init p
    >|= fun store -> (h, Authorization None, B.time (), store)

  let terminated (_,s,_,_) = (s = Disconnected)

  let auth_fail hostname store banner_time =
    ((hostname, Authorization None, banner_time, store), Reply.err None)

  let auth_quit hostname store banner_time =
    ((hostname, Disconnected, banner_time, store), Reply.ok None [])

  let auth_result hostname store banner_time mailbox success =
    if success then
      ((hostname, Transaction mailbox, banner_time, store),
        Reply.ok (Some mailbox) [])
    else auth_fail hostname store banner_time

  let auth_apop hostname store banner_time mailbox digest =
    S.apop_of_mailbox store banner_time hostname mailbox
    >|= fun digest_option ->
      match digest_option with
      | None ->
        auth_fail hostname store banner_time
      | Some digest' ->
        auth_result hostname store banner_time mailbox (digest = digest')

  let auth_pass hostname store banner_time mailbox secret =
    S.secret_of_mailbox store mailbox
    >|= fun secret_option ->
      match secret_option with
      | None ->
        auth_fail hostname store banner_time
      | Some secret' ->
        auth_result hostname store banner_time mailbox (secret = secret')

  let auth_user hostname store banner_time mailbox =
    Lwt.return
      ((hostname, Authorization (Some mailbox), banner_time, store),
        Reply.ok (Some mailbox) [])

  let f_auth_none hostname store banner_time cmd =
    match cmd with
    | Apop (mailbox, digest) ->
      auth_apop hostname store banner_time mailbox digest
    | Quit ->
      Lwt.return (auth_quit hostname store banner_time)
    | User mailbox ->
      auth_user hostname store banner_time mailbox
    | _ ->
      Lwt.return (auth_fail hostname store banner_time)

  let f_auth_some hostname store banner_time mailbox cmd =
    match cmd with
    | Pass secret ->
      auth_pass hostname store banner_time mailbox secret
    | Quit ->
      Lwt.return (auth_quit hostname store banner_time)
    | _ ->
      Lwt.return (auth_fail hostname store banner_time)

  let f (hostname, state, banner_time, store) cmd =
    match state with
    | Authorization None ->
      f_auth_none hostname store banner_time cmd
    | Authorization (Some mailbox) ->
      f_auth_some hostname store banner_time mailbox cmd
    | Disconnected ->
      Lwt.return ((hostname, Disconnected, banner_time, store), Reply.internal_error)
    | Transaction _ ->
      Lwt.return ((hostname, Disconnected, banner_time, store), Reply.internal_error)
    (*| Update _ ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)*)
end
