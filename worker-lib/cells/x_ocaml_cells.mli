(** What the worker keeps about its cells: the phrase being run, and which cell
    each display and callback belongs to. {!X_ocaml_lib} is what code in a cell
    calls; this is what the worker and {!X_ocaml_lib} call. Cells cannot see it:
    the worker exports {!X_ocaml_lib} to them, and not this. *)

val set_phrase : cell:int -> loc:int -> unit
(** The phrase about to run: its cell, and where it ends in the cell's code,
    which is where its output goes. *)

val forget_cell : int -> unit
(** The cell is about to run again: its displays go, its callbacks are
    forgotten, and then each display's [on_remove] runs, as part of the cell. *)

val invoke : int -> string -> unit
(** [invoke token payload] runs the callback [token] names, as part of its cell.
    An unknown token does nothing. *)

val set_source : string -> line_number:int -> string -> unit
(** [set_source name ~line_number code]: the cell whose data-id is [name] is
    about to run [code], which starts at line [line_number] of the page's
    cells. *)

val run_after_phrase : unit -> unit
(** The phrase being run has finished: run what {!after_phrase} asked for. *)

(** {1 For X_ocaml_lib, and libraries in the worker} *)

val source : string -> (int * string) option
(** The code of the cell whose data-id is the name, as it was last run, and
    the line it starts at; [None] if no such cell has run. *)

val after_phrase : (unit -> unit) -> unit
(** [after_phrase f] runs [f] once the phrase being run has finished, after
    the toplevel has set its environment for it. A library that runs phrases
    of its own, from code in a cell, needs it: the toplevel then forgets what
    they defined. *)

val post : X_protocol.response -> unit
val cell : unit -> int
val phrase_end : unit -> int

val add_display : (unit -> unit) -> int
(** A new display of the running cell, with its [on_remove]. *)

val live : int -> bool
val remove : int -> unit

val add_callback : (string -> unit) -> int
(** A new callback of the running cell. *)
