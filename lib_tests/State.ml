open Pop3.State
open Pop3.Store

let digest = "<1514764801.0@>abc"

let mailbox = "123"

let secret = "abc"

module ConstBanner : Banner = struct
  let time () = Unix.gmtime 1514764801.0
end

module NoopStore : Store = struct
  type t = unit

  let init _p = Lwt.return ()

  let secret_of_mailbox _s _m =
    Lwt.return (Some secret)

  let apop_of_mailbox _s _t _h _m =
    Lwt.return (Some digest)
end

let unit_tests = [
]