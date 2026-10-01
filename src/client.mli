type t

val make : ?extra_load:string -> string -> t
val on_message : t -> (X_protocol.response -> unit) -> unit
val post : t -> X_protocol.request -> unit
val eval : ?verbatim:bool -> id:int -> line_number:int -> t -> string -> unit
(** [verbatim]: run the code as written, without reformatting it first. *)
val fmt : id:int -> t -> string -> unit

val restart : t -> unit
(** Stop the worker, whatever it is doing, and start a fresh one, which has none
    of the old one's state. *)
