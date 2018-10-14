(** Status indicators in server replies indicate success or failure. *)
type status_indicator =
  | Ok (** Success indicator. *)
  | Error (** Failure indicator. *)

(** Serializes values of type [status_indicator] according to RFC
    specifications.
    
    @return '+OK' for [Ok] and '-ERR' for [Error]. *)
val string_of_status_indicator : status_indicator -> string