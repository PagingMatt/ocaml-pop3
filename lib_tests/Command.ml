open Pop3.Command

module Helpers = struct
  let validate_none (cmd:t option) =
    match cmd with
    | None   -> ignore ()
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'None'."

  let validate_some_apop (cmd:t option) e_a e_b =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Apop (a, b)) ->
      Alcotest.(check string)
        "Checking first argument of APOP." e_a a;
      Alcotest.(check string)
        "Checking second argument of APOP." e_b b
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Apop ...)'."

  let validate_some_dele (cmd:t option) e_i =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Dele i) ->
      Alcotest.(check int)
        "Checking argument of DELE." e_i i
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Dele ...)'."

  let validate_some_list_none (cmd:t option) =
    match cmd with
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

  let validate_some_list_some (cmd:t option) e_i =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (List (Some i)) ->
      Alcotest.(check int)
        "Checking argument of LIST." e_i i
    | Some (List (None)) ->
      Alcotest.fail
        "t_of_string_opt returned 'Some (List (None))' but expected 'Some (List (Some _))'."
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (List ...)'."

  let validate_some_noop (cmd:t option) =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Noop) -> ignore ()
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Noop)'."

  let validate_some_pass (cmd:t option) e_a =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Pass a) ->
      Alcotest.(check string)
        "Checking argument of PASS." e_a a
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Pass ...)'."

  let validate_some_quit (cmd:t option) =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Quit) -> ignore ()
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Quit)'."

  let validate_some_retr (cmd:t option) e_i =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Retr i) ->
      Alcotest.(check int)
        "Checking argument of RETR." e_i i
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Retr ...)'."

  let validate_some_rset (cmd:t option) =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Rset) -> ignore ()
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Rset)'."

  let validate_some_stat (cmd:t option) =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Stat) -> ignore ()
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Stat)'."

  let validate_some_top (cmd:t option) e_i e_j =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Top (i, j)) ->
      Alcotest.(check int)
        "Checking first argument of TOP." e_i i;
      Alcotest.(check int)
        "Checking second argument of TOP" e_j j
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Top ...)'."

  let validate_some_uidl_none (cmd:t option) =
    match cmd with
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

  let validate_some_uidl_some (cmd:t option) e_i =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (Uidl (Some i)) ->
      Alcotest.(check int)
        "Checking argument of UIDL" e_i i
    | Some (Uidl (None)) ->
      Alcotest.fail
        "t_of_string_opt returned 'Some (Uidl (None))' but expected 'Some (Uidl (Some _))'."
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (Uidl ...)'."

  let validate_some_user (cmd:t option) e_a =
    match cmd with
    | None ->
      Alcotest.fail
        "t_of_string_opt returned 'None' but expected 'Some ...'."
    | Some (User a) ->
      Alcotest.(check string)
        "Checking argument of USER" e_a a
    | Some _ ->
      Alcotest.fail
        "t_of_string_opt returned 'Some ...' but expected 'Some (User ...)'."
end

let t_of_string_opt_apop_valid () =
  let cmd = "APOP A B" in
  Helpers.validate_some_apop (t_of_string_opt cmd) "A" "B"

let t_of_string_opt_apop_no_args_invalid () =
  let cmd = "APOP" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_apop_one_arg_invalid () =
  let cmd = "APOP A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_apop_three_args_invalid () =
  let cmd = "APOP A B C" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_dele_valid () =
  let cmd = "DELE 1" in
  Helpers.validate_some_dele (t_of_string_opt cmd) 1

let t_of_string_opt_dele_no_args_invalid () =
  let cmd = "DELE" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_dele_not_int_arg_invalid () =
  let cmd = "DELE A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_dele_two_args_invalid () =
  let cmd = "DELE 1 2" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_list_valid_none () =
  let cmd = "LIST" in
  Helpers.validate_some_list_none (t_of_string_opt cmd)

let t_of_string_opt_list_valid_some () =
  let cmd = "LIST 1" in
  Helpers.validate_some_list_some (t_of_string_opt cmd) 1

let t_of_string_opt_list_not_int_arg_invalid () =
  let cmd = "LIST A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_list_two_args_invalid () =
  let cmd = "LIST 1 2" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_noop_valid () =
  let cmd = "NOOP" in
  Helpers.validate_some_noop (t_of_string_opt cmd)

let t_of_string_opt_noop_one_arg_invalid () =
  let cmd = "NOOP A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_pass_valid () =
  let cmd = "PASS A" in
  Helpers.validate_some_pass (t_of_string_opt cmd) "A"

let t_of_string_opt_pass_no_args_invalid () =
  let cmd = "PASS" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_pass_two_args_invalid () =
  let cmd = "PASS A B" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_quit_valid () =
  let cmd = "QUIT" in
  Helpers.validate_some_quit (t_of_string_opt cmd)

let t_of_string_opt_quit_one_arg_invalid () =
  let cmd = "QUIT A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_retr_valid () =
  let cmd = "RETR 1" in
  Helpers.validate_some_retr (t_of_string_opt cmd) 1

let t_of_string_opt_retr_no_args_invalid () =
  let cmd = "RETR" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_retr_not_int_arg_invalid () =
  let cmd = "RETR A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_rset_valid () =
  let cmd = "RSET" in
  Helpers.validate_some_rset (t_of_string_opt cmd)

let t_of_string_opt_rset_one_arg_invalid () =
  let cmd = "RSET A" in
  Helpers.validate_none (t_of_string_opt cmd)
    
let t_of_string_opt_stat_valid () =
  let cmd = "STAT" in
  Helpers.validate_some_stat (t_of_string_opt cmd)

let t_of_string_opt_stat_one_arg_invalid () =
  let cmd = "STAT A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_valid () =
  let cmd = "TOP 1 2" in
  Helpers.validate_some_top (t_of_string_opt cmd) 1 2

let t_of_string_opt_top_no_args_invalid () =
  let cmd = "TOP" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_one_int_arg_invalid () =
  let cmd = "TOP 1" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_one_not_int_arg_invalid () =
  let cmd = "TOP A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_two_not_int_args_invalid () =
  let cmd = "TOP A B" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_int_not_int_arg_invalid () =
  let cmd = "TOP 1 A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_not_int_int_arg_invalid () =
  let cmd = "TOP A 1" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_top_three_int_args_invalid () =
  let cmd = "TOP 1 2 3" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_uidl_valid_none () =
  let cmd = "UIDL" in
  Helpers.validate_some_uidl_none (t_of_string_opt cmd)

let t_of_string_opt_uidl_valid_some () =
  let cmd = "UIDL 1" in
  Helpers.validate_some_uidl_some (t_of_string_opt cmd) 1

let t_of_string_opt_uidl_not_int_arg_invalid () =
  let cmd = "UIDL A" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_uidl_two_args_invalid () =
  let cmd = "UIDL 1 2" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_user_valid () =
  let cmd = "USER A" in
  Helpers.validate_some_user (t_of_string_opt cmd) "A"

let t_of_string_opt_user_no_args_invalid () =
  let cmd = "USER" in
  Helpers.validate_none (t_of_string_opt cmd)

let t_of_string_opt_user_two_args_invalid () =
  let cmd = "USER A B" in
  Helpers.validate_none (t_of_string_opt cmd)

let unit_tests = [
  ("Checks that valid APOP command parsed correctly"                        , `Quick, t_of_string_opt_apop_valid                  );
  ("Checks that valid DELE command parsed correctly"                        , `Quick, t_of_string_opt_dele_valid                  );
  ("Checks that valid LIST (None) command parsed correctly"                 , `Quick, t_of_string_opt_list_valid_none             );
  ("Checks that valid LIST (Some) command parsed correctly"                 , `Quick, t_of_string_opt_list_valid_some             );
  ("Checks that valid NOOP command parsed correctly"                        , `Quick, t_of_string_opt_noop_valid                  );
  ("Checks that valid PASS command parsed correctly"                        , `Quick, t_of_string_opt_pass_valid                  );
  ("Checks that valid QUIT command parsed correctly"                        , `Quick, t_of_string_opt_quit_valid                  );
  ("Checks that valid RETR command parsed correctly"                        , `Quick, t_of_string_opt_retr_valid                  );
  ("Checks that valid RSET command parsed correctly"                        , `Quick, t_of_string_opt_rset_valid                  );
  ("Checks that valid STAT command parsed correctly"                        , `Quick, t_of_string_opt_stat_valid                  );
  ("Checks that valid TOP command parsed correctly"                         , `Quick, t_of_string_opt_top_valid                   );
  ("Checks that valid UIDL (None) command parsed correctly"                 , `Quick, t_of_string_opt_uidl_valid_none             );
  ("Checks that valid UIDL (Some) command parsed correctly"                 , `Quick, t_of_string_opt_uidl_valid_some             );
  ("Checks that valid USER command parsed correctly"                        , `Quick, t_of_string_opt_user_valid                  );
  ("Checks that APOP command with no arguments returns None"                , `Quick, t_of_string_opt_apop_no_args_invalid        );
  ("Checks that APOP command with one argument returns None"                , `Quick, t_of_string_opt_apop_one_arg_invalid        );
  ("Checks that APOP command with three arguments returns None"             , `Quick, t_of_string_opt_apop_three_args_invalid     );
  ("Checks that DELE command with no arguments returns None"                , `Quick, t_of_string_opt_dele_no_args_invalid        );
  ("Checks that DELE command with non-int argument returns None"            , `Quick, t_of_string_opt_dele_not_int_arg_invalid    );
  ("Checks that DELE command with two arguments returns None"               , `Quick, t_of_string_opt_dele_two_args_invalid       );
  ("Checks that LIST command with non-int argument returns None"            , `Quick, t_of_string_opt_list_not_int_arg_invalid    );
  ("Checks that LIST command with two arguments returns None"               , `Quick, t_of_string_opt_list_two_args_invalid       );
  ("Checks that NOOP command with one argument returns None"                , `Quick, t_of_string_opt_noop_one_arg_invalid        );
  ("Checks that PASS command with no arguments returns None"                , `Quick, t_of_string_opt_pass_no_args_invalid        );
  ("Checks that PASS command with two arguments returns None"               , `Quick, t_of_string_opt_pass_two_args_invalid       );
  ("Checks that QUIT command with one argument returns None"                , `Quick, t_of_string_opt_quit_one_arg_invalid        );
  ("Checks that RETR command with no arguments returns None"                , `Quick, t_of_string_opt_retr_no_args_invalid        );
  ("Checks that RETR command with non-int argument returns None"            , `Quick, t_of_string_opt_retr_not_int_arg_invalid    );
  ("Checks that RSET command with one argument returns None"                , `Quick, t_of_string_opt_rset_one_arg_invalid        );
  ("Checks that STAT command with one argument returns None"                , `Quick, t_of_string_opt_stat_one_arg_invalid        );
  ("Checks that TOP command with no arguments returns None"                 , `Quick, t_of_string_opt_top_no_args_invalid         );
  ("Checks that TOP command with one int argument returns None"             , `Quick, t_of_string_opt_top_one_int_arg_invalid     );
  ("Checks that TOP command with one not-int argument returns None"         , `Quick, t_of_string_opt_top_one_not_int_arg_invalid );
  ("Checks that TOP command with two not-int arguments returns None"        , `Quick, t_of_string_opt_top_two_not_int_args_invalid);
  ("Checks that TOP command with one int, one not-int argument returns None", `Quick, t_of_string_opt_top_int_not_int_arg_invalid );
  ("Checks that TOP command with one not-int, one int argument returns None", `Quick, t_of_string_opt_top_not_int_int_arg_invalid );
  ("Checks that TOP command with three int arguments returns None"          , `Quick, t_of_string_opt_top_three_int_args_invalid  );
  ("Checks that UIDL command with non-int argument returns None"            , `Quick, t_of_string_opt_uidl_not_int_arg_invalid    );
  ("Checks that UIDL command with two arguments returns None"               , `Quick, t_of_string_opt_uidl_two_args_invalid       );
  ("Checks that USER command with no arguments returns None"                , `Quick, t_of_string_opt_user_no_args_invalid        );
  ("Checks that USER command with two arguments returns None"               , `Quick, t_of_string_opt_user_two_args_invalid       );
]