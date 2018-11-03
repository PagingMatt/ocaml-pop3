let unit_test_suite = [
  ("Message unit tests", Message.unit_tests);
  ("State unit tests"  , State.unit_tests  );
]

let () =
  Alcotest.run "pop3-server unit tests" unit_test_suite