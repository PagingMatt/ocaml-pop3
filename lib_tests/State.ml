open Lwt.Infix
open Pop3.Command
open Pop3.State
open Pop3.Store

let digest = "<1514764801.0@>abc"

let mailbox = "123"

let secret = "abc"

module ConstBanner : Banner = struct
  let time () = Unix.gmtime 1514764801.0
end

module NoopOkStore : Store = struct
  type t = unit

  let init _p = Lwt.return ()

  let secret_of_mailbox _s _m =
    Lwt.return (Some secret)

  let apop_of_mailbox _s _t _h _m =
    Lwt.return (Some digest)
end

module NoopErrStore : Store = struct
  type t = unit

  let init _p = Lwt.return ()

  let secret_of_mailbox _s _m =
    Lwt.return (Some secret)

  let apop_of_mailbox _s _t _h _m =
    Lwt.return (Some digest)
end

module TestOkState = BackingStoreState (ConstBanner) (NoopOkStore)

module TestErrState = BackingStoreState (ConstBanner) (NoopErrStore)

let f_auth_none_user_ok_mailbox_reply switch () =
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
  let cmd = User mailbox in
  TestOkState.start ""
  >>= fun s -> TestOkState.f s cmd
  >|= fun (_,r) ->
    Alcotest.(check string) "Checking reply."
      "+OK 123" (Pop3.Reply.string_of_t r)

let f_auth_none_apop_ok_mailbox_reply switch () =
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
  let cmd = Apop (mailbox, digest) in
  TestOkState.start ""
  >>= fun s -> TestOkState.f s cmd
  >|= fun (_,r) ->
    Alcotest.(check string) "Checking reply."
      "+OK 123" (Pop3.Reply.string_of_t r)

let f_auth_some_pass_ok_mailbox_reply switch () =
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
  let cmd_user = User mailbox in
  let cmd_pass = Pass secret in
  TestOkState.start ""
  >>= fun s -> TestOkState.f s cmd_user
  >>= fun (s',_) -> TestOkState.f s' cmd_pass
  >|= fun (_ ,r) ->
    Alcotest.(check string) "Checking reply."
      "+OK 123" (Pop3.Reply.string_of_t r)

let f_auth_some_apop_err_reply switch () =
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
  let cmd_user = User mailbox in
  let cmd_apop = Apop (mailbox, digest) in
  TestOkState.start ""
  >>= fun s -> TestOkState.f s cmd_user
  >>= fun (s',_) -> TestOkState.f s' cmd_apop
  >|= fun (_ ,r) ->
    Alcotest.(check string) "Checking reply."
      "-ERR" (Pop3.Reply.string_of_t r)

let unit_tests = [
  Alcotest_lwt.test_case "Check reply from valid USER command." `Quick f_auth_none_user_ok_mailbox_reply;
  Alcotest_lwt.test_case "Check reply from valid APOP command." `Quick f_auth_none_user_ok_mailbox_reply;
  Alcotest_lwt.test_case "Check reply from valid PASS command." `Quick f_auth_some_pass_ok_mailbox_reply;
  Alcotest_lwt.test_case "Check reply from invalid APOP command (following a USER command)." `Quick f_auth_some_apop_err_reply;
]