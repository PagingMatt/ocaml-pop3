open Lwt.Infix
open Pop3.Command
open Pop3.State
open Pop3.Store

module Helpers = struct
  let digest  = "<1514764801.0@>abc"
  let mailbox = "123"
  let secret  = "abc"

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
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_apop
      >|= fun (_,r) ->
        Alcotest.(check string) "Checking reply."
          "+OK 123" (Pop3.Reply.string_of_t r)

    let f_auth_none_pass_err_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_pass
      >|= fun (_,r) ->
        Alcotest.(check string) "Checking reply."
          "-ERR" (Pop3.Reply.string_of_t r)

    let f_auth_none_user_ok_mailbox_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_user
      >|= fun (_,r) ->
        Alcotest.(check string) "Checking reply."
          "+OK 123" (Pop3.Reply.string_of_t r)

    let unit_tests = [
      Alcotest_lwt.test_case "Check reply from valid APOP command." `Quick f_auth_none_user_ok_mailbox_reply;
      Alcotest_lwt.test_case "Check reply from invalid PASS command (not following a USER command)." `Quick f_auth_none_pass_err_reply;
      Alcotest_lwt.test_case "Check reply from valid USER command." `Quick f_auth_none_user_ok_mailbox_reply;
    ]
  end

  module Some = struct
    open Helpers

    let f_auth_some_apop_err_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_user
      >>= fun (s',_) -> TestOkState.f s' cmd_apop
      >|= fun (_ ,r) ->
        Alcotest.(check string) "Checking reply."
          "-ERR" (Pop3.Reply.string_of_t r)

    let f_auth_some_pass_ok_mailbox_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_user
      >>= fun (s',_) -> TestOkState.f s' cmd_pass
      >|= fun (_ ,r) ->
        Alcotest.(check string) "Checking reply."
          "+OK 123" (Pop3.Reply.string_of_t r)

    let f_auth_some_user_err_reply switch () =
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return ());
      TestOkState.start ""
      >>= fun s -> TestOkState.f s cmd_user
      >>= fun (s',_) -> TestOkState.f s' cmd_user
      >|= fun (_ ,r) ->
        Alcotest.(check string) "Checking reply."
          "-ERR" (Pop3.Reply.string_of_t r)

    let unit_tests = [
      Alcotest_lwt.test_case "Check reply from invalid APOP command (following a USER command)." `Quick f_auth_some_apop_err_reply;
      Alcotest_lwt.test_case "Check reply from valid PASS command." `Quick f_auth_some_pass_ok_mailbox_reply;
      Alcotest_lwt.test_case "Check reply from invalid USER command (following a USER command)." `Quick f_auth_some_user_err_reply;
    ]
  end
end

let unit_tests =
  Authorization.None.unit_tests @
  Authorization.Some.unit_tests