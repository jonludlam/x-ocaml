(** What runs a page's code: x-ocaml's own worker, or a js_top_worker toplevel
    with a universe of packages. *)

type t

val builtin : ?extra_load:string -> string -> t
(** [builtin worker_url]: x-ocaml's worker. *)

val jtw : ?findlib_requires:string list -> ?findlib_index:string -> string -> t
(** [jtw worker_url]: a js_top_worker toplevel. *)

val on_message : t -> (X_protocol.response -> unit) -> unit
val post : t -> X_protocol.request -> unit
val eval : ?owner:Brr.El.t -> id:int -> line_number:int -> t -> string -> unit
(** [owner] is the cell's element, which widgets the code makes follow. *)
val fmt : id:int -> t -> string -> unit

val restart : t -> unit
(** Stop the worker, whatever it is doing, and start a fresh one, with none of
    the old one's state. *)
