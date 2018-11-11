open Message

module type Store = sig
  type t

  val init : string -> t Lwt.t

  val secret_of_mailbox : t -> string -> string option Lwt.t

  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t

  val message_list_of_mailbox : t -> string -> int list Lwt.t

  val lines_of_message : t -> string -> int -> string list option Lwt.t

  val uid_of_message : t -> string -> int -> string option Lwt.t
end

module type IrminStringKv =
  Irmin.KV with type contents = Irmin.Contents.String.t

module IrminStore (S : IrminStringKv) (P : MessageParser) : Store = struct
  open Lwt.Infix

  type t = S.t

  let init p =
    Irmin_git.config ~bare:false p
    |> S.Repo.v
    >>= S.master

  let secret_of_mailbox s m =
    S.find s ["secrets"; m]

  let apop_of_mailbox s t h m =
    secret_of_mailbox s m
    >|= fun secret_option ->
      match secret_option with
      | None -> None
      | Some secret ->
        let ft, _ = Unix.mktime t in
        Printf.sprintf "<%f@%s>%s" ft h secret
        |> Digest.string
        |> fun digest -> Some digest

  let message_list_of_mailbox s m =
    S.list s ["mailboxes"; m]
    >|= List.filter
      (fun (num, ls) ->
        match num |> int_of_string_opt with
        | Some _ ->
          (match ls with
          | `Contents -> true
          | `Node     -> false)
        | None   -> false)
    >|= List.map
      (fun (num, _) -> int_of_string num)

  let read_message s m i =
    string_of_int i
    |> fun msg -> S.find s ["mailboxes"; m; msg]

  let lines_of_message s m i =
    read_message s m i
    >|= fun contents ->
      match contents with
      | None -> None
      | Some s -> P.lines_of_string s

  let uid_of_message s m i =
    read_message s m i
    >|= fun contents ->
      match contents with
      | None -> None
      | Some s -> P.uid_of_string s
end
