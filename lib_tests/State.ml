open Lwt.Infix
open Pop3.Command
open Pop3.State
open Pop3.Store

module Helpers = struct
  let digest   = "<1514764801.0@localhost>abc"
  let digest'  = "<1514764801.0@localhost>def"
  let hostname = "localhost"
  let mailbox  = "123"
  let maildrop = "/tmp/pop3"
  let secret   = "abc"
  let secret'  = "def"

  module ConstBanner : Banner = struct
    let time () = Unix.gmtime 1514764801.0
  end

  module NoopStoreA : Store = struct
    type t = unit

    let init _p = Lwt.return ()

    let secret_of_mailbox _s _m =
      Lwt.return (Some secret)

    let apop_of_mailbox _s _t _h _m =
      Lwt.return (Some digest)
  end

  module NoopStoreB : Store = struct
    type t = unit

    let init _p = Lwt.return ()

    let secret_of_mailbox _s _m =
      Lwt.return (Some secret')

    let apop_of_mailbox _s _t _h _m =
      Lwt.return (Some digest')
  end

  module NoopStoreErr : Store = struct
    type t = unit

    let init _p = Lwt.return ()

    let secret_of_mailbox _s _m =
      Lwt.return None

    let apop_of_mailbox _s _t _h _m =
      Lwt.return None
  end

  module TestStateA = BackingStoreState (ConstBanner) (NoopStoreA)

  module TestStateB = BackingStoreState (ConstBanner) (NoopStoreB)

  module TestStateErr = BackingStoreState (ConstBanner) (NoopStoreErr)

  let cmd_apop  = Apop (mailbox, digest)
  let cmd_dele  = Dele 0
  let cmd_list  = List None
  let cmd_list' = List (Some 0)
  let cmd_noop  = Noop
  let cmd_pass  = Pass secret
  let cmd_quit  = Quit
  let cmd_retr  = Retr 0
  let cmd_rset  = Rset
  let cmd_stat  = Stat
  let cmd_top   = Top (0, 1)
  let cmd_uidl  = Uidl None
  let cmd_uidl' = Uidl (Some 0)
  let cmd_user  = User mailbox
end

module Authorization = struct
  module None = struct
    open Helpers

    let f_auth_none_apop_ok_mailbox_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_apop
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "+OK 123" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_none_apop_err_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateB.start hostname maildrop
      >>= fun s -> TestStateB.f s cmd_apop
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_none_apop_err_reply' switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateErr.start hostname maildrop
      >>= fun s -> TestStateErr.f s cmd_apop
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_none_quit_ok_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_quit
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "+OK" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_none_user_ok_mailbox_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_user
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "+OK 123" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_none_other_cmd_err_reply cmd switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd
      >|= fun (_,r) ->
        match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let unit_tests = [
      Alcotest_lwt.test_case "Check reply from valid APOP command in 'Authorization None'."                            `Quick (f_auth_none_apop_ok_mailbox_reply);
      Alcotest_lwt.test_case "Check reply from invalid APOP command in 'Authorization None' (wrong secret in digest)." `Quick (f_auth_none_apop_err_reply);
      Alcotest_lwt.test_case "Check reply from invalid APOP command in 'Authorization None' (no secret in store)."     `Quick (f_auth_none_apop_err_reply');
      Alcotest_lwt.test_case "Check reply from invalid DELE command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_dele);
      Alcotest_lwt.test_case "Check reply from invalid LIST (None) command in 'Authorization None'."                   `Quick (f_auth_none_other_cmd_err_reply cmd_list);
      Alcotest_lwt.test_case "Check reply from invalid LIST (Some) command in 'Authorization None'."                   `Quick (f_auth_none_other_cmd_err_reply cmd_list');
      Alcotest_lwt.test_case "Check reply from invalid NOOP command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_noop);
      Alcotest_lwt.test_case "Check reply from valid QUIT command in 'Authorization None'."                            `Quick (f_auth_none_quit_ok_reply);
      Alcotest_lwt.test_case "Check reply from invalid PASS command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_pass);
      Alcotest_lwt.test_case "Check reply from invalid RETR command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_retr);
      Alcotest_lwt.test_case "Check reply from invalid RSET command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_rset);
      Alcotest_lwt.test_case "Check reply from invalid STAT command in 'Authorization None'."                          `Quick (f_auth_none_other_cmd_err_reply cmd_stat);
      Alcotest_lwt.test_case "Check reply from invalid TOP command in 'Authorization None'."                           `Quick (f_auth_none_other_cmd_err_reply cmd_top);
      Alcotest_lwt.test_case "Check reply from invalid UIDL (None) command in 'Authorization None'."                   `Quick (f_auth_none_other_cmd_err_reply cmd_uidl);
      Alcotest_lwt.test_case "Check reply from invalid UIDL (Some) command in 'Authorization None'."                   `Quick (f_auth_none_other_cmd_err_reply cmd_uidl');
      Alcotest_lwt.test_case "Check reply from valid USER command in 'Authorization None'."                            `Quick (f_auth_none_user_ok_mailbox_reply);
    ]
  end

  module Some = struct
    open Helpers

    let f_auth_some_pass_ok_mailbox_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_user
      >>= fun (s',_) -> TestStateA.f s' cmd_pass
      >|= fun (_ ,r) ->
      match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "+OK 123" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_some_pass_err_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateB.start hostname maildrop
      >>= fun s -> TestStateB.f s cmd_user
      >>= fun (s',_) -> TestStateB.f s' cmd_pass
      >|= fun (_ ,r) ->
      match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_some_pass_err_reply' switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateErr.start hostname maildrop
      >>= fun s -> TestStateErr.f s cmd_user
      >>= fun (s',_) -> TestStateErr.f s' cmd_pass
      >|= fun (_ ,r) ->
      match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_some_quit_ok_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_user
      >>= fun (s',_) -> TestStateA.f s' cmd_quit
      >|= fun (_ ,r) ->
      match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "+OK" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let f_auth_some_other_cmd_err_reply cmd switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestStateA.start hostname maildrop
      >>= fun s -> TestStateA.f s cmd_user
      >>= fun (s',_) -> TestStateA.f s' cmd
      >|= fun (_ ,r) ->
      match Pop3.Reply.lines_of_t r with
        | l::[] ->
          Alcotest.(check string) "Checking reply."
            "-ERR" l
        | _ -> Alcotest.fail "Unexpected reply lines pattern."

    let unit_tests = [
      Alcotest_lwt.test_case "Check reply from invalid APOP command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_apop);
      Alcotest_lwt.test_case "Check reply from invalid DELE command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_dele);
      Alcotest_lwt.test_case "Check reply from invalid LIST (None) command in 'Authorization (Some mailbox)'."               `Quick (f_auth_some_other_cmd_err_reply cmd_list);
      Alcotest_lwt.test_case "Check reply from invalid LIST (Some) command in 'Authorization (Some mailbox)'."               `Quick (f_auth_some_other_cmd_err_reply cmd_list');
      Alcotest_lwt.test_case "Check reply from invalid NOOP command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_noop);
      Alcotest_lwt.test_case "Check reply from valid QUIT command in 'Authorization (Some mailbox)'."                        `Quick (f_auth_some_quit_ok_reply);
      Alcotest_lwt.test_case "Check reply from valid PASS command in 'Authorization (Some mailbox)'."                        `Quick (f_auth_some_pass_ok_mailbox_reply);
      Alcotest_lwt.test_case "Check reply from invalid PASS command in 'Authorization (Some mailbox)' (wrong secret)."       `Quick (f_auth_some_pass_err_reply);
      Alcotest_lwt.test_case "Check reply from invalid PASS command in 'Authorization (Some mailbox)' (no secret in store)." `Quick (f_auth_some_pass_err_reply');
      Alcotest_lwt.test_case "Check reply from invalid RETR command in 'Authorization (Some mailbox)' ."                     `Quick (f_auth_some_other_cmd_err_reply cmd_retr);
      Alcotest_lwt.test_case "Check reply from invalid RSET command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_rset);
      Alcotest_lwt.test_case "Check reply from invalid STAT command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_stat);
      Alcotest_lwt.test_case "Check reply from invalid TOP command in 'Authorization (Some mailbox)'."                       `Quick (f_auth_some_other_cmd_err_reply cmd_top);
      Alcotest_lwt.test_case "Check reply from invalid UIDL (None) command in 'Authorization (Some mailbox)'."               `Quick (f_auth_some_other_cmd_err_reply cmd_uidl);
      Alcotest_lwt.test_case "Check reply from invalid UIDL (Some) command in 'Authorization (Some mailbox)'."               `Quick (f_auth_some_other_cmd_err_reply cmd_uidl');
      Alcotest_lwt.test_case "Check reply from invalid USER command in 'Authorization (Some mailbox)'."                      `Quick (f_auth_some_other_cmd_err_reply cmd_user);
    ]
  end
end

let unit_tests =
  Authorization.None.unit_tests @
  Authorization.Some.unit_tests