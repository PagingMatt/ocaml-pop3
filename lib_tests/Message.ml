open Pop3.Message.JsonMessageParser

let lines_of_string_some_line () =
  let ls = lines_of_string "[\"A\"]" in
  match ls with
  | Some (l::[]) ->
    Alcotest.(check string) "Checks value of single deserialized line."
      "A" l
  | _ -> Alcotest.fail "Unexpected parsed lines pattern."

let lines_of_string_some_lines () =
  let ls = lines_of_string "[\"A\",\"B\"]" in
  match ls with
  | Some (l1::l2::[]) ->
    Alcotest.(check string) "Checks value of first deserialized line."
      "A" l1;
    Alcotest.(check string) "Checks value of second deserialized line."
      "B" l2;
  | _ -> Alcotest.fail "Unexpected parsed lines pattern."

let unit_tests = [
  Alcotest.test_case "Checks value of single deserialized line." `Quick lines_of_string_some_line;
  Alcotest.test_case "Checks value of deserialized lines."       `Quick lines_of_string_some_lines;
]
