open Command
open Unix

type authorization_state = tm * string option
type transaction_state   = string
type update_state        = string

type 'a command_result = {
  state : 'a;
  reply : Reply.t;
  next : bool;
}

module type Authorizer = sig
  val authorize :
    authorization_state -> Command.t -> authorization_state command_result
end

module type Transactor = sig
  val transact :
    transaction_state -> Command.t -> transaction_state command_result
end

module type Updater = sig
  val update :
    update_state -> Command.t -> update_state command_result
end

module State (A : Authorizer) (T : Transactor) (U : Updater) : sig
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  val start : unit -> t

  val f : t -> Command.t -> t
end = struct
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  let start () = Authorization ((time () |> gmtime), None)

  let f state cmd =
    match state with
    | Disconnected -> Disconnected
    | Authorization (time, auth_state) ->
      (match auth_state with
      | Some mailbox ->
        (match cmd with
        | Pass _password -> Transaction mailbox
        | Quit -> Disconnected
        | _ -> state)
      | None ->
        (match cmd with
        | Apop (mailbox, _digest) -> Transaction mailbox
        | Quit -> Disconnected
        | User mailbox -> Authorization (time, (Some mailbox))
        | _ -> state))
    | Transaction mailbox ->
      (match cmd with
      | Dele _message -> Transaction mailbox
      | List _  -> Transaction mailbox
      | Noop -> Transaction mailbox
      | Quit -> Update mailbox
      | Retr _message -> Transaction mailbox
      | Rset -> Transaction mailbox
      | Stat -> Transaction mailbox
      | Top (_message, _lines) -> Transaction mailbox
      | Uidl _message -> Transaction mailbox
      | _ -> Transaction mailbox)
    | Update mailbox ->
      (match cmd with
      | Quit -> Update mailbox
      | _ -> Update mailbox)
end
