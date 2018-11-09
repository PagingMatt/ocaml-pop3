open State

module type Server = functor (S : State) -> sig
  val start : hostname:string -> maildrop:string -> stop:unit Lwt.t
    -> unit Lwt.t
end
