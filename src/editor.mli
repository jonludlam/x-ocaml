type t

val make : ?read_only:bool -> Brr.El.t -> t
val source : t -> string
val set_source : t -> string -> unit
val clear : t -> unit
val nb_lines : t -> int
val count_lines : string -> int
val get_previous_lines : t -> int
val set_previous_lines : t -> int -> unit
val clear_messages : t -> unit
val add_message : t -> int -> Brr.El.t list -> unit
val on_change : t -> (unit -> unit) -> unit
val configure_merlin : t -> (unit -> Code_mirror.State.Extension.t list) -> unit

val set_layout : t -> fold:(int * int) list -> mark:(int * int) list -> unit
(** [set_layout t ~fold ~mark] folds each range of lines [(first, last)] in
    [fold] behind its first line, and marks the lines in [mark]'s ranges (as a
    page's [fold] and [highlight] attributes ask). Lines count from 1 within
    the cell; ranges that don't fit the source are left out. *)
