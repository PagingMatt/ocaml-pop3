open Pop3.Reply

let lines_of_t_error_none () =
  match err None |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking serialization of 'Error None'."
      "-ERR" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_error_some () =
  match err (Some "A") |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking serialization of 'Error (Some 'A')'."
      "-ERR A" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_none_empty () =
  match ok None [] |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking serialization of 'Ok (None, [])'."
      "+OK" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_none_lines () =
  match ok None ["A"; "B"] |> lines_of_t with
  | l1::l2::l3::_l4::[] ->
    Alcotest.(check string) "Checking line 1 serialization of 'Ok (None, ['A'; 'B'])'."
      "+OK" l1;
    Alcotest.(check string) "Checking line 2 serialization of 'Ok (None, ['A'; 'B'])'."
      "A" l2;
    Alcotest.(check string) "Checking line 3 serialization of 'Ok (None, ['A'; 'B'])'."
      "B" l3;
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_none_lines_last_terminating_octet () =
  match ok None ["A"; "B"] |> lines_of_t with
  | _l1::_l2::_l3::l4::[] ->
    Alcotest.(check string) "Checking line 4 serialization of 'Ok (None, ['A'; 'B'])'."
      "." l4
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_some_empty () =
  match ok (Some "A") [] |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking serialization of 'Ok (Some 'A', [])'."
      "+OK A" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_some_lines () =
  match ok (Some "A") ["B"; "C"] |> lines_of_t with
  | l1::l2::l3::_l4::[] ->
    Alcotest.(check string) "Checking line 1 serialization of 'Ok (None, ['A'; 'B'])'."
      "+OK A" l1;
    Alcotest.(check string) "Checking line 2 serialization of 'Ok (None, ['A'; 'B'])'."
      "B" l2;
    Alcotest.(check string) "Checking line 3 serialization of 'Ok (None, ['A'; 'B'])'."
      "C" l3;
  | _ -> Alcotest.fail "Unexpected line pattern."

let lines_of_t_ok_some_lines_last_terminating_octet () =
  match ok (Some "A") ["B"; "C"] |> lines_of_t with
  | _l1::_l2::_l3::l4::[] ->
    Alcotest.(check string) "Checking line 4 serialization of 'Ok (None, ['A'; 'B'])'."
      "." l4
  | _ -> Alcotest.fail "Unexpected line pattern."

let greeting_is_ok_some_greeting () =
  let t = Unix.gmtime 1514764801. in
  let h = "localhost" in
  match Common.greeting h t |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking 'greeting' is 'OK (Some 'server ready <1514764801.0@localhost>')'."
      "+OK server ready <1514764801.000000@localhost>" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let internal_error_is_error_none () =
  match Common.internal_error |> lines_of_t with
  | l::[] ->
    Alcotest.(check string) "Checking 'internal_error' is 'Error None'."
      "-ERR" l
  | _ -> Alcotest.fail "Unexpected line pattern."

let unit_tests = [
  ("Checking serialization of 'Error None'"                                          , `Quick, lines_of_t_error_none);
  ("Checking serialization of 'Error (Some 'A')'"                                    , `Quick, lines_of_t_error_some);
  ("Checking serialization of 'Ok (None, [])'"                                       , `Quick, lines_of_t_ok_none_empty);
  ("Checking serialization of 'Ok (None, ['A'; 'B'])'"                               , `Quick, lines_of_t_ok_none_lines);
  ("Checking terminating character of 'Ok (None, ['A'; 'B'])'"                       , `Quick, lines_of_t_ok_none_lines_last_terminating_octet);
  ("Checking serialization of 'Ok (Some 'A', [])'"                                   , `Quick, lines_of_t_ok_some_empty);
  ("Checking serialization of 'Ok (Some 'A', ['B'; 'C'])'"                           , `Quick, lines_of_t_ok_some_lines);
  ("Checking terminating character of 'Ok (Some 'A', ['B'; 'C'])'"                   , `Quick, lines_of_t_ok_some_lines_last_terminating_octet);
  ("Checking 'Reply.greeting' is 'OK (Some 'server ready <1514764801.0@localhost>')'", `Quick, greeting_is_ok_some_greeting);
  ("Checking 'Reply.internal_error' is 'Error None'"                                 , `Quick, internal_error_is_error_none);
]
