(** POP3 server. *)

open State

(** The [Server] functor is applied to some session state machine module. *)
module Server (S : State) : sig
  (** Starts a TCP server listening on port 110 to serve POP3 client
      connections.

      There is no limit to the number of concurrent connections and connections
      do not timeout.

      @param [hostname] is the hostname of the server used in APOP digests.

      @param [maildrop] is the path to the maildrop on disk.

      @param [stop] is the lightweight thread which if realised can terminate
             the server.

      @return unit lightweight thread running the server. *)
  val start : hostname:string -> maildrop:string -> stop:unit Lwt.t
    -> unit Lwt.t
end
