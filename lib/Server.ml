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
      reply
      |> Reply.lines_of_t
      |> Lwt_list.iter_s (Lwt_io.write_line output_channel)
      >>= fun () -> iter_state input_channel output_channel state')

  let callback hostname maildrop _flow input_channel output_channel =
    Lwt_list.iter_s (Lwt_io.write_line output_channel)
      (Reply.Common.greeting |> Reply.lines_of_t)
    >>= fun () -> S.start hostname maildrop
    >>= iter_state input_channel output_channel

  let start ~hostname ~maildrop ~stop =
    let ctx = default_ctx in
    let mode = `TCP (`Port 110) in
    Conduit_lwt_unix.serve ~stop ~ctx ~mode (callback hostname maildrop)
end