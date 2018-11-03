(** POP3 server. *)

open Pop3_server.Server
open Pop3_server.State

(** The [Pop3Server] functor is applied to some session state machine. *)
module Pop3Server (S : State) : Server
