(** All minimal and optional POP3 commands. *)
type t =
  | Apop of string * string
  | Dele of int
  | List of int option
  | Noop
  | Pass of string
  | Quit
  | Retr of int
  | Rset
  | Stat
  | Top of int * int
  | Uidl of int option
  | User of string

(** Deserializes a [t] from a [string] without raising.

    @return a [t option] dependent on whether the command passed in was a valid
            command.*)
val t_of_string_opt : string -> t option
