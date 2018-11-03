open Conduit_lwt_unix
open Lwt.Infix
open Pop3
open Pop3_server.Server
open Pop3_server.State

module Pop3Server (S : State) : Server = struct
  let rec iter_state input_channel output_channel state =
    if S.terminated state then Lwt.return () else
    Lwt_io.read_line input_channel
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
    S.start hostname maildrop
    >>= fun state -> (
      S.banner_time state
      |> Reply.Common.greeting hostname
      |> Reply.lines_of_t
      |> Lwt_list.iter_s (Lwt_io.write_line output_channel)
      >>= fun () ->
        try
          iter_state input_channel output_channel state
        with
        (* Client has disconnected ungracefully. *)
        | Unix.Unix_error(Unix.ECONNRESET, _, _) -> Lwt.return ())

  let on_exn exn =
    match exn with
    (* EOF indicates that the underlying channels are now unavailable after
       connection closes from ungraceful client disconnect. *)
    | End_of_file -> ()
    (* Other unexpected exceptions should be re-thrown. *)
    | _ as e      -> raise e

  let start ~hostname ~maildrop ~stop =
    let ctx = default_ctx in
    let mode = `TCP (`Port 110) in
    Conduit_lwt_unix.serve ~stop ~on_exn ~ctx ~mode (callback hostname maildrop)
end