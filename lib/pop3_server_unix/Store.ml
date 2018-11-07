open Pop3_server.Message
open Pop3_server.Store

module IrminUnixStringKv = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

module IrminUnixStore : Store = IrminStore (IrminUnixStringKv) (JsonMessageParser)
