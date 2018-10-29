open Lwt.Infix
open Message

module type Store = sig
  type t

  val init : string -> t Lwt.t

  val secret_of_mailbox : t -> string -> string option Lwt.t

  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t

  val lines_of_message : t -> string -> int -> string list option Lwt.t
end

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

  let lines_of_message s m i =
    string_of_int i
    |> fun msg -> IrminGitFsKvStore.find s ["mailboxes"; m; msg]
    >|= fun contents ->
      match contents with
      | None -> None
      | Some s -> P.lines_of_string s
end
