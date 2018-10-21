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

let unit_tests = [
  Alcotest_lwt.test_case "a" `Quick f_auth_none_user_ok_mailbox_reply
]