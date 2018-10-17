open Unix

type authorization_state =
  | None
  | Mailbox of string

type t =
  | Authorization of tm * authorization_state
  | Transaction of string
  | Update of string
