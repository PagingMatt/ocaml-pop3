(** POP3 server. *)

open State

(** The [Server] functor is applied to some session state machine module. *)
module Server (S : State) : sig
  (** Starts a TCP server listening on port 110 to serve POP3 client connections.

      There is no limit to the number of concurrent connections and connections
      do not timeout.

      The parameter passed to the function is a lightweight thread which will
      kill the server should it become determined.

      @return unit lightweight thread. *)
  val start : unit Lwt.t -> unit Lwt.t
end
