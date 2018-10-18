open Lwt.Infix
open Unix

type authorization_state = | Banner of tm | Mailbox of tm * string | Quit
type transaction_state   = string
type update_state        = string

type 'a command_result = {
  state : 'a;
  reply : Reply.t;
  next : bool;
}

module type Authorizer = sig
  val authorize :
    authorization_state -> Command.t
      -> (authorization_state command_result) Lwt.t
end

module type Transactor = sig
  val transact :
    transaction_state -> Command.t
      -> (transaction_state command_result) Lwt.t
end

module type Updater = sig
  val update :
    update_state -> Command.t
      -> (update_state command_result) Lwt.t
end

module State (A : Authorizer) (T : Transactor) (U : Updater) : sig
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  val start : unit -> t

  val f : t -> Command.t -> t Lwt.t
end = struct
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  let start () = Authorization (Banner (time () |> gmtime))

  let f state cmd =
    match state with
    | Disconnected -> Lwt.return Disconnected
    | Authorization auth_state ->
      A.authorize auth_state cmd >|= fun res ->
      (match res.state with
      | Banner t -> Authorization (Banner t)
      | Mailbox (t, m) ->
        if res.next then Transaction m else Authorization (Mailbox (t, m))
      | Quit -> Disconnected)
    | Transaction trans_state ->
      T.transact trans_state cmd >|= fun res ->
      if res.next then Update res.state else Transaction res.state
    | Update update_state ->
      U.update update_state cmd >|= fun res ->
      if res.next then Disconnected else Update res.state
end
