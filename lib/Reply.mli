(** Replies from POP3 server to client commands. *)

(** Replies themselves are a variant of '+OK' and '-ERR' with additional
    messages attached. *)
type t =
  | Ok of string option * (string list)
  (** In the '+OK' case there is an optional first line message and a list of
      additional message lines. *)
  | Error of string option
  (** In the '-ERR' case there is an optional first line message, but no
      subsequent message lines. *)

(** Serializes values of [t] according to RFC specifications.
    
    @return '+OK ...' or '-ERR ...'. *)
val string_of_t : t -> string
