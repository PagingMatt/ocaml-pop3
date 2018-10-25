open Lwt.Infix
open Store

module type Banner = sig
  val time : unit -> Unix.tm
end

module GmTimeBanner : Banner = struct
  let time () =
    Unix.time () |> Unix.gmtime
end

module type State = sig
  type t

  val start : string -> string -> t Lwt.t

  val banner_time : t -> Unix.tm

  val terminated : t -> bool

  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end

module BackingStoreState (B : Banner) (S : Store) : State = struct
  open Command

  type pop3_session_state =
    | Disconnected
    | Authorization of string option
    | Transaction   of string
    | Update        of string

  type t = string * pop3_session_state * Unix.tm * S.t

  let start h p =
    S.init p
    >|= fun store -> (h, Authorization None, B.time (), store)

  let banner_time (_,_,t,_) = t

  let terminated (_,s,_,_) = (s = Disconnected)

  let auth_fail hostname store banner_time =
    ((hostname, Authorization None, banner_time, store),
      Reply.err (Some "permission denied"))

  let auth_invalid_cmd hostname store banner_time =
    ((hostname, Authorization None, banner_time, store),
      Reply.err (Some "command invalid before authorized"))

  let auth_quit hostname store banner_time =
    ((hostname, Disconnected, banner_time, store),
      Reply.ok (Some (Printf.sprintf "%s POP3 server signing off" hostname)) [])

  let auth_result hostname store banner_time mailbox success =
    if success then
      ((hostname, Transaction mailbox, banner_time, store),
        Reply.ok (Some "maildrop locked and ready") [])
    else auth_fail hostname store banner_time

  let auth_apop hostname store banner_time mailbox digest =
    S.apop_of_mailbox store banner_time hostname mailbox
    >|= fun digest_option ->
      match digest_option with
      | None ->
        auth_fail hostname store banner_time
      | Some digest' ->
        auth_result hostname store banner_time mailbox (digest = digest')

  let auth_pass hostname store banner_time mailbox_opt secret =
    match mailbox_opt with
    | None -> Lwt.return (auth_fail hostname store banner_time)
    | Some mailbox ->
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

  let f_auth hostname store banner_time mailbox cmd =
    match cmd with
    | Apop (mailbox', digest) ->
      (* Attempt to verify digest, if this fails reset memoized mailbox. *)
      auth_apop hostname store banner_time mailbox' digest
    | Pass secret ->
      (* Attempt to verify secret for memoized mailbox. *)
      auth_pass hostname store banner_time mailbox secret
    | Quit ->
      (* Terminate the session. *)
      Lwt.return (auth_quit hostname store banner_time)
    | User mailbox' ->
      (* Memoize mailbox and return +OK ready for PASS command. *)
      auth_user hostname store banner_time mailbox'
    | _ ->
      (* Other commands are invalid in auth state. *)
      Lwt.return (auth_invalid_cmd hostname store banner_time)

  let trans_fail hostname store banner_time mailbox =
    ((hostname, Transaction mailbox, banner_time, store), Reply.err None)

  let trans_quit hostname store banner_time mailbox =
    ((hostname, Update mailbox, banner_time, store), Reply.ok None [])

  let trans_retr hostname store banner_time mailbox msg =
    S.read store mailbox msg
    >|= fun ls_option ->
      match ls_option with
      | None -> trans_fail hostname store banner_time mailbox
      | Some ls ->
        ((hostname, Transaction mailbox, banner_time, store),
          Reply.ok (Some "-1 octets") ls)

  let f_trans hostname store banner_time mailbox cmd =
    match cmd with
    | Quit -> Lwt.return (trans_quit hostname store banner_time mailbox)
    | Retr msg -> trans_retr hostname store banner_time mailbox msg
    | _ -> Lwt.return (trans_fail hostname store banner_time mailbox)

  let f (hostname, state, banner_time, store) cmd =
    match state with
    | Authorization mailbox ->
      f_auth hostname store banner_time mailbox cmd
    | Disconnected ->
      Lwt.return ((hostname, Disconnected, banner_time, store), Reply.Common.internal_error)
    | Transaction mailbox ->
      f_trans hostname store banner_time mailbox cmd
    | Update _ ->
      Lwt.return ((hostname, Disconnected, banner_time, store), Reply.Common.internal_error)
end
