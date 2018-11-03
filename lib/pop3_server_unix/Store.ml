open Lwt.Infix
open Pop3_server.Message
open Pop3_server.Store

module IrminStore (P : MessageParser) : Store = struct
  module IrminGitFsKvStore = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

  type t = IrminGitFsKvStore.t

  let init p =
    Irmin_git.config ~bare:false p
    |> IrminGitFsKvStore.Repo.v
    >>= IrminGitFsKvStore.master

  let secret_of_mailbox s m =
    IrminGitFsKvStore.find s ["secrets"; m]

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

  let read_message s m i =
    string_of_int i
    |> fun msg -> IrminGitFsKvStore.find s ["mailboxes"; m; msg]

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
