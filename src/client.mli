type t

val make : ?extra_load:string -> string -> t
val on_message : t -> (X_protocol.response -> unit) -> unit
val post : t -> X_protocol.request -> unit

val transfer : t -> token:int -> payload:string -> Jv.t -> unit
(** [transfer t ~token ~payload v] calls back [token] with [payload] and [v],
    which is transferred to the worker (an [OffscreenCanvas], say), not
    marshalled. *)

val eval : id:int -> line_number:int -> t -> string -> unit
val fmt : id:int -> t -> string -> unit

val restart : t -> unit
(** Stop the worker, whatever it is doing, and start a fresh one, which has none
    of the old one's state. *)
