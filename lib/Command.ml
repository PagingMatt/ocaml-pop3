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

let parse_apop args =
  match args with
  | (a1::a2::[]) -> Some (Apop (a1, a2))
  | _            -> None

let parse_dele args =
  match args with
  | (a1::[]) ->
    (match (int_of_string_opt a1) with
    | Some i -> Some (Dele i)
    | None   -> None)
  | _ -> None

let parse_list args =
  match args with
  | (a1::[]) ->
    (match (int_of_string_opt a1) with
    | Some i -> Some (List (Some i))
    | None   -> None)
  | [] -> Some (List None)
  | _  -> None

let parse_noop args =
  match args with
  | [] -> Some Noop
  | _  -> None

let parse_pass args =
  match args with
  | (a1::[]) -> Some (Pass a1)
  | _        -> None

let parse_quit args =
  match args with
  | [] -> Some Quit
  | _  -> None

let parse_retr args =
  match args with
  | (a1::[]) ->
    (match (int_of_string_opt a1) with
    | Some i -> Some (Retr i)
    | None   -> None)
  | _  -> None

let parse_rset args =
  match args with
  | [] -> Some Noop
  | _  -> None

let parse_stat args =
  match args with
  | [] -> Some Quit
  | _  -> None

let parse_top args =
  match args with
  | (a1::a2::[]) ->
    (match (int_of_string_opt a1) with
    | None    -> None
    | Some i1 ->
    match (int_of_string_opt a2) with
    | None    -> None
    | Some i2 -> Some (Top (i1, i2)))
  | _ -> None

let parse_uidl args =
  match args with
  | (a1::[]) ->
    (match (int_of_string_opt a1) with
    | Some i -> Some (Uidl (Some i))
    | None   -> None)
  | [] -> Some (Uidl None)
  | _  -> None

let parse_user args =
  match args with
  | (a1::[]) -> Some (User a1)
  | _        -> None

let parse cmd args =
  if cmd = "APOP" then parse_apop args else
  if cmd = "DELE" then parse_dele args else
  if cmd = "LIST" then parse_list args else
  if cmd = "NOOP" then parse_noop args else
  if cmd = "PASS" then parse_pass args else
  if cmd = "QUIT" then parse_quit args else
  if cmd = "RETR" then parse_retr args else
  if cmd = "RSET" then parse_rset args else
  if cmd = "STAT" then parse_stat args else
  if cmd = "TOP"  then parse_top  args else
  if cmd = "UIDL" then parse_uidl args else
  if cmd = "USER" then parse_user args else
  None

let t_of_string_opt cmd =
  let cmd_args = String.uppercase_ascii cmd |> String.split_on_char ' ' in
  match cmd_args with
  | c::args -> parse c args
  | _       -> None
