(** Replies from POP3 server to client commands. *)

(** Reply type. *)
type t

(** Serializes values of [t] according to RFC specifications. Each list element
    corresponds to a line in the reply. The first list element will always
    include the status indicator for the reply.

    @return '+OK ...' or '-ERR ...'. *)
val lines_of_t : t -> string list

(** Common replies. *)
module Common : sig
  (** Greeting from server when a new client connects. *)
  val greeting : t

  (** Reply value to indicate an internal server error. *)
  val internal_error : t
end

(** Constructor for '+OK ...' replies. *)
val ok : string option -> string list -> t

(** Constructor for '-ERR ...' replies. *)
val err : string option -> t