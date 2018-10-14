type t =
  | Apop of string * string
  | Dele of int
  | List of int option
  | Noop
  | Quit
  | Retr of int
  | Rset
  | Stat
  | Top of int * int
  | Uidl of int option
  | User of string
  | Pass of string
  