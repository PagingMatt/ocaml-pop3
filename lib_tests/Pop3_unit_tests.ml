let unit_test_suite = [
  ("Command unit tests", Command.unit_tests);
]

let () =
  Alcotest.run "pop3 unit tests" unit_test_suite