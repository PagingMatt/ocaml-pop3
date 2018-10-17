open Pop3.Command

let t_of_string_opt_apop_valid () =
  let cmd = "APOP a b" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Apop (a, b)) ->
    Alcotest.(check string)
      "First element of tuple for APOP expected to be 'A'."
      "A" a;
    Alcotest.(check string)
      "Second element of tuple for APOP expected to be 'B'."
      "B" b
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Apop ...)'."

let t_of_string_opt_dele_valid () =
  let cmd = "DELE 1" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Dele i) ->
    Alcotest.(check int)
      "Argument of DELE expected to be '1'."
      1 i
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Dele ...)'."

let t_of_string_opt_list_valid_none () =
  let cmd = "LIST" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (List (None)) -> ignore ()
  | Some (List (Some _)) -> 
    Alcotest.fail
      "t_of_string_opt returned 'Some (List (Some _))' but expected 'Some (List (None))'."
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (List ...)'."

let t_of_string_opt_list_valid_some () =
  let cmd = "LIST 1" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (List (Some i)) ->
    Alcotest.(check int)
      "Argument of LIST expected to be '1'."
      1 i
  | Some (List (None)) ->
    Alcotest.fail
      "t_of_string_opt returned 'Some (List (None))' but expected 'Some (List (Some 1))'."
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (List ...)'."

let t_of_string_opt_noop_valid () =
  let cmd = "NOOP" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Noop) -> ignore ()
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Noop)'."

let t_of_string_opt_pass_valid () =
  let cmd = "PASS a" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Pass a) ->
    Alcotest.(check string)
      "Argument of PASS expected to be 'A'."
      "A" a
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Pass ...)'."

let t_of_string_opt_quit_valid () =
  let cmd = "QUIT" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Quit) -> ignore ()
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Quit)'."

let t_of_string_opt_retr_valid () =
  let cmd = "RETR 1" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Retr i) ->
    Alcotest.(check int)
      "Argument of RETR expected to be '1'."
      1 i
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Retr ...)'."

let t_of_string_opt_rset_valid () =
  let cmd = "RSET" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Rset) -> ignore ()
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Rset)'."
    
let t_of_string_opt_stat_valid () =
  let cmd = "STAT" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Stat) -> ignore ()
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Stat)'."

let t_of_string_opt_top_valid () =
  let cmd = "TOP 1 2" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Top (i, j)) ->
    Alcotest.(check int)
      "First element of tuple for TOP expected to be '1'."
      1 i;
    Alcotest.(check int)
      "Second element of tuple for TOP expected to be '2'."
      2 j
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Top ...)'."

let t_of_string_opt_uidl_valid_none () =
  let cmd = "UIDL" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Uidl (None)) -> ignore ()
  | Some (Uidl (Some _)) -> 
    Alcotest.fail
      "t_of_string_opt returned 'Some (Uidl (Some _))' but expected 'Some (Uidl (None))'."
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Uidl ...)'."

let t_of_string_opt_uidl_valid_some () =
  let cmd = "UIDL 1" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (Uidl (Some i)) ->
    Alcotest.(check int)
      "Argument of Uidl expected to be '1'."
      1 i
  | Some (Uidl (None)) ->
    Alcotest.fail
      "t_of_string_opt returned 'Some (Uidl (None))' but expected 'Some (Uidl (Some 1))'."
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (Uidl ...)'."

let t_of_string_opt_user_valid () =
  let cmd = "USER a" in
  match (t_of_string_opt cmd) with
  | None ->
    Alcotest.fail
      "t_of_string_opt returned 'None' but expected 'Some ...'."
  | Some (User a) ->
    Alcotest.(check string)
      "Argument of USER expected to be 'A'."
      "A" a
  | Some _ ->
    Alcotest.fail
      "t_of_string_opt returned 'Some ...' but expected 'Some (User ...)'."

let unit_tests = [
  ("Checks that valid APOP command parsed correctly"       , `Quick, t_of_string_opt_apop_valid     );
  ("Checks that valid DELE command parsed correctly"       , `Quick, t_of_string_opt_dele_valid     );
  ("Checks that valid LIST (None) command parsed correctly", `Quick, t_of_string_opt_list_valid_none);
  ("Checks that valid LIST (Some) command parsed correctly", `Quick, t_of_string_opt_list_valid_some);
  ("Checks that valid NOOP command parsed correctly"       , `Quick, t_of_string_opt_noop_valid     );
  ("Checks that valid PASS command parsed correctly"       , `Quick, t_of_string_opt_pass_valid     );
  ("Checks that valid QUIT command parsed correctly"       , `Quick, t_of_string_opt_quit_valid     );
  ("Checks that valid RETR command parsed correctly"       , `Quick, t_of_string_opt_retr_valid     );
  ("Checks that valid RSET command parsed correctly"       , `Quick, t_of_string_opt_rset_valid     );
  ("Checks that valid STAT command parsed correctly"       , `Quick, t_of_string_opt_stat_valid     );
  ("Checks that valid TOP command parsed correctly"        , `Quick, t_of_string_opt_top_valid      );
  ("Checks that valid UIDL (None) command parsed correctly", `Quick, t_of_string_opt_uidl_valid_none);
  ("Checks that valid UIDL (Some) command parsed correctly", `Quick, t_of_string_opt_uidl_valid_some);
  ("Checks that valid USER command parsed correctly"       , `Quick, t_of_string_opt_user_valid     );
]