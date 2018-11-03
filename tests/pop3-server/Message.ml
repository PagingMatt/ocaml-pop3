open Pop3_server.Message.JsonMessageParser

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

let lines_of_string_int_list_none () =
  let ls = lines_of_string "[1, 2]" in
  match ls with
  | None -> ignore ()
  | _ -> Alcotest.fail "Unexpected parsed lines pattern."

let lines_of_string_mixed_list_none () =
  let ls = lines_of_string "[\"A\", 2]" in
  match ls with
  | None -> ignore ()
  | _ -> Alcotest.fail "Unexpected parsed lines pattern."

let uid_of_string_single_disgest_of_single () =
  let uid_opt = uid_of_string "[\"A\"]" in
  match uid_opt with
  | Some uid ->
    Alcotest.(check string) "Checks value of UID."
      (Digest.string "A" |> Digest.to_hex) uid
  | None -> Alcotest.fail "Unexpected UID pattern."

let uid_of_string_lines_disgest_of_lines () =
  let uid_opt = uid_of_string "[\"A\",\"B\",\"C\"]" in
  match uid_opt with
  | Some uid ->
    Alcotest.(check string) "Checks value of UID."
      (Digest.string "A\r\nB\r\nC" |> Digest.to_hex) uid
  | None -> Alcotest.fail "Unexpected UID pattern."

let unit_tests = [
  Alcotest.test_case "Checks value of single deserialized line." `Quick lines_of_string_some_line;
  Alcotest.test_case "Checks value of deserialized lines."       `Quick lines_of_string_some_lines;
  Alcotest.test_case "Checks int list deserializes to None."     `Quick lines_of_string_int_list_none;
  Alcotest.test_case "Checks mixed list deserializes to None."   `Quick lines_of_string_mixed_list_none;
  Alcotest.test_case "Checks UID of single line."                `Quick uid_of_string_single_disgest_of_single;
  Alcotest.test_case "Checks UID of lines."                      `Quick uid_of_string_lines_disgest_of_lines;
]
