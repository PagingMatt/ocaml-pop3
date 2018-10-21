(** Replies that can be issued by a POP3 server in response to a POP3 client. *)

(** Replies. *)
type t

(** Serializes values of [t] according to RFC specifications. Each list element
    corresponds to a line in the reply. The first list element will always
    include the status indicator for the reply.

    @return '+OK ...' or '-ERR ...'. *)
val lines_of_t : t -> string list

(** Common replies. *)
module Common : sig
  (** Greeting from server when a new client connects. The greeting is dependent
      on the banner time of the session and the hostname of the server. *)
  val greeting : string -> Unix.tm -> t

  (** Reply value to indicate an internal server error. *)
  val internal_error : t
end

(** Constructor for '+OK ...' replies. *)
val ok : string option -> string list -> t

(** Constructor for '-ERR ...' replies. *)
val err : string option -> t