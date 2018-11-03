module type Server = sig
  val start : hostname:string -> maildrop:string -> stop:unit Lwt.t
    -> unit Lwt.t
end
