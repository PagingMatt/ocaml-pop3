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

  let f_auth banner_time store cmd =
    match cmd with
    | User mailbox ->
      Lwt.return
        ((Authorization (Some mailbox), banner_time, store), Reply.ok (Some mailbox) [])
    | Apop (mailbox, digest) ->
      S.apop_of_mailbox store banner_time "" mailbox
      >|= fun digest_option ->
        (match digest_option with
        | None -> ((Authorization None, banner_time, store), Reply.err None)
        | Some digest' ->
          if digest = digest' then
          ((Transaction mailbox, banner_time, store), Reply.ok (Some mailbox) []) else
          ((Authorization None, banner_time, store), Reply.err None))
    | _ -> Lwt.return ((Authorization None, banner_time, store), Reply.err None)

  let f (state, banner_time, store) cmd =
    match state with
    | Authorization None -> f_auth banner_time store cmd
    | Authorization (Some _mailbox) ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)
    | Disconnected ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)
    | Transaction _ ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)
    (*| Update _ ->
      Lwt.return ((Disconnected, banner_time, store), Reply.internal_error)*)
end
