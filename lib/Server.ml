open Conduit_lwt_unix
open Lwt.Infix

open State

module Server (S : State) : sig
  val start : unit Lwt.t -> unit Lwt.t
end = struct
  let callback _flow _input_channel output_channel =
    Lwt_io.write output_channel (Reply.greeting |> Reply.string_of_t)
    (* TODO: implement state machine representing server. *)
    >|= ignore

  let start (stop:unit Lwt.t) =
    let ctx = default_ctx in
    let mode = `TCP (`Port 110) in
    Conduit_lwt_unix.serve ~stop ~ctx ~mode callback
end