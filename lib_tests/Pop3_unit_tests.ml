let unit_test_suite = [
  ("Command unit tests", Command.unit_tests);
  ("Message unit tests", Message.unit_tests);
  ("Reply unit tests"  , Reply.unit_tests  );
  ("State unit tests"  , State.unit_tests  );
]

let () =
  Alcotest.run "pop3 unit tests" unit_test_suite