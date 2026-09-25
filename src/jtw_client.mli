(** x-ocaml's messages to and from a js_top_worker toplevel.

    js_top_worker runs the code and answers merlin's queries itself, from the
    packages of a universe: cmis and compiled libraries described by a
    [findlib_index.json]. *)

type t

val make : string -> t
(** [make worker_url] starts the worker. *)

val init : ?findlib_requires:string list -> ?findlib_index:string -> t -> unit
(** Configures the worker with the universe at [findlib_index] and loads
    [findlib_requires] from it. *)

val on_message : t -> (X_protocol.response -> unit) -> unit
val post : t -> X_protocol.request -> unit
val eval : id:int -> line_number:int -> t -> string -> unit
val fmt : id:int -> t -> string -> unit

val restart : t -> unit
(** A fresh worker, configured as the old one was. *)
