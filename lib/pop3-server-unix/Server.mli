(** POP3 server. *)

open Pop3_server.Server

(** The [Pop3UnixServer] functor is applied to some session state machine. *)
module Pop3UnixServer : Server
