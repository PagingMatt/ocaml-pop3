(** States that the server can be in for a POP3 session. *)
type t =
  | NoConnection
  | Authorization
  | Transaction
  | Update
