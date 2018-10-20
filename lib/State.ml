open Store
open Unix

type authorization_state = | Banner of tm | Mailbox of tm * string | Quit
type transaction_state   = string
type update_state        = string

type 'a command_result = {
  state : 'a;
  reply : Reply.t;
  next : bool;
}

module type Banner = sig
  val get_time : unit -> tm
end

module GmTimeBanner : Banner = struct
  let get_time () =
    time () |> gmtime
end

module State (B : Banner) (S : Store) : sig
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  val start : unit -> t

  val f : t -> Command.t -> (t * Reply.t) Lwt.t
end = struct
  type t =
    | Disconnected
    | Authorization of authorization_state
    | Transaction   of transaction_state
    | Update        of update_state

  let start () = Authorization (Banner (B.get_time ()))

  let f state _cmd =
    match state with
    | Disconnected ->
      Lwt.return (Disconnected, Reply.internal_error)
    | Authorization _auth_state ->
      Lwt.return (Disconnected, Reply.internal_error)
    | Transaction _trans_state ->
      Lwt.return (Disconnected, Reply.internal_error)
    | Update _update_state ->
      Lwt.return (Disconnected, Reply.internal_error)
end
