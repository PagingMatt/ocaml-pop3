open Pop3.Reply

let string_of_t_error_none () =
  let reply = err None in
  Alcotest.(check string) "Checking serialization of 'Error None'."
  "-ERR" (string_of_t reply)

let string_of_t_error_some () =
  let reply = err (Some "A") in
  Alcotest.(check string) "Checking serialization of 'Error (Some 'A')'."
  "-ERR A" (string_of_t reply)

let string_of_t_ok_none_empty () =
  let reply = ok None [] in
  Alcotest.(check string) "Checking serialization of 'Ok (None, [])'."
  "+OK" (string_of_t reply)

let string_of_t_ok_none_lines () =
  let reply = ok None ["A"; "B"] in
  Alcotest.(check string) "Checking serialization of 'Ok (None, ['A'; 'B'])'."
  "+OK\r\nA\r\nB" (string_of_t reply)

let string_of_t_ok_some_empty () =
  let reply = ok (Some "A") [] in
  Alcotest.(check string) "Checking serialization of 'Ok (Some 'A', [])'."
  "+OK A" (string_of_t reply)

let string_of_t_ok_some_lines () =
  let reply = ok (Some "A") ["B"; "C"] in
  Alcotest.(check string) "Checking serialization of 'Ok (Some 'A', ['B'; 'C'])'."
  "+OK A\r\nB\r\nC" (string_of_t reply)

let internal_error_is_error_none () =
  let reply = internal_error in
  Alcotest.(check string) "Checking 'internal_error' is 'Error None'."
  "-ERR" (string_of_t reply)

let unit_tests = [
  ("Checking serialization of 'Error None'"               , `Quick, string_of_t_error_none      );
  ("Checking serialization of 'Error (Some 'A')'"         , `Quick, string_of_t_error_some      );
  ("Checking serialization of 'Ok (None, [])'"            , `Quick, string_of_t_ok_none_empty   );
  ("Checking serialization of 'Ok (None, ['A'; 'B'])'"    , `Quick, string_of_t_ok_none_lines   );
  ("Checking serialization of 'Ok (Some 'A', [])'"        , `Quick, string_of_t_ok_some_empty   );
  ("Checking serialization of 'Ok (Some 'A', ['B'; 'C'])'", `Quick, string_of_t_ok_some_lines   );
  ("Checking 'Reply.internal_error' is 'Error None'"      , `Quick, internal_error_is_error_none);
]
