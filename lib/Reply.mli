(** Replies from POP3 server to client commands. *)

(** Replies themselves are a variant of '+OK' and '-ERR' with additional
    messages attached. *)
type t

(** Serializes values of [t] according to RFC specifications.

    @return '+OK ...' or '-ERR ...'. *)
val string_of_t : t -> string

(** Reply value to indicate an internal server error. *)
val internal_error : t

(** In the '+OK' case there is an optional first line message and a list of
      additional message lines. *)
val ok : string option -> string list -> t

(** In the '-ERR' case there is an optional first line message, but no
      subsequent message lines. *)
val err : string option -> t