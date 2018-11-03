(** Store of mailbox secrets and maildrop content. *)

open Pop3_server.Message
open Pop3_server.Store

(** The [IrminStore] module is an implementation of the [Store] signature which
    uses Irmin for the underlying store. *)
module IrminStore (P : MessageParser) : Store
