open Conduit_lwt_unix
open Lwt.Infix

open State

module Server (S : State) : sig
  val start : hostname:string -> maildrop:string -> stop:unit Lwt.t
    -> unit Lwt.t
end = struct
  let rec iter_state input_channel output_channel state =
    if S.terminated state then Lwt.return () else
    Lwt_io.read input_channel
    >|= Command.t_of_string_opt
    >>= fun cmd_opt ->
      (match cmd_opt with
      | None -> Lwt.return (state, Reply.err None)
      | Some cmd -> S.f state cmd)
    >>= (fun (state', reply) ->
      Lwt_io.write output_channel (reply |> Reply.string_of_t)
      >>= fun () -> iter_state input_channel output_channel state')

  let callback hostname maildrop _flow input_channel output_channel =
    Lwt_io.write output_channel (Reply.greeting |> Reply.string_of_t)
    >>= fun () -> S.start hostname maildrop
    >>= iter_state input_channel output_channel

  let start ~hostname ~maildrop ~stop =
    let ctx = default_ctx in
    let mode = `TCP (`Port 110) in
    Conduit_lwt_unix.serve ~stop ~ctx ~mode (callback hostname maildrop)
end