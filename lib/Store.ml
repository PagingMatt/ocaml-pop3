open Lwt.Infix

module type Store = sig
  type t

  val init : string -> t Lwt.t

  val secret_of_mailbox : t -> string -> string option Lwt.t

  val apop_of_mailbox : t -> Unix.tm -> string -> string -> string option Lwt.t
end

module IrminStore : Store = struct
  module IrminMenSecretStore = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

  type t = IrminMenSecretStore.t

  let init p =
    Irmin_git.config ~bare:true p
    |> IrminMenSecretStore.Repo.v
    >>= IrminMenSecretStore.master

  let secret_of_mailbox s m =
    IrminMenSecretStore.find s ["secrets"; m]

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
end
