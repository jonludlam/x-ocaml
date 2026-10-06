(** How a cell shows and treats its code:
    - [Interactive], the default: an editor whose code runs in turn.
    - [Exercise]: the same, marked out as code the reader is to write.
    - [Test]: read-only, folded behind a summary that shows whether it passed,
      and run by the page each time its exercise completes.
    - [Hidden]: no editor at all; its code still runs, and later cells and
      merlin see its definitions. *)
type mode = Interactive | Exercise | Test | Hidden

val mode_of_string : string -> mode
(** From a [mode] attribute; anything unrecognised is [Interactive]. *)

type t

type group
(** Cells that are one program (a [group] attribute): their code runs
    together, joined in page order, when the last of them runs; messages go
    to the member whose code they are about. *)

val make_group : size:int -> group
(** A group of [size] cells, as many as the page has with that name. *)

val init :
  id:int ->
  mode:mode ->
  run_on:[ `Click | `Load ] ->
  ?merlin:bool ->
  ?cell_id:string ->
  ?extra_style:Jstr.t ->
  ?inline_style:Jstr.t ->
  ?group:group * int ->
  eval_fn:(id:int -> line_number:int -> string -> unit) ->
  fmt_fn:(id:int -> string -> unit) ->
  post_fn:(X_protocol.request -> unit) ->
  Webcomponent.t ->
  t

val id : t -> int
val mode : t -> mode

val has_completed : t -> bool
(** The cell has run since it last changed. *)

val set_on_completed : t -> (t -> unit) -> unit
(** [set_on_completed cell f]: [f] runs each time [cell] finishes running, in
    place of any function set before. *)

val set_on_stale : t -> (bool -> unit) -> unit
(** [set_on_stale cell f]: [f true] when the cell's code, or code before
    it, changes after it has run, so that what it showed is out of date;
    [f false] when it runs again. *)

val set_stop : t -> (unit -> unit) -> unit
(** [set_stop cell f]: [f] runs when the reader stops [cell] while it runs; it
    replaces any function set before. *)

val reset_status : t -> unit
(** Forget that the cell ran: its output goes and it is as if never run. *)

val set_source : t -> string -> unit
val add_message : t -> int -> X_protocol.output list -> unit
val completed_run : t -> X_protocol.output list -> unit
val set_prev : prev:t option -> t -> unit
val receive_merlin : t -> Protocol.answer -> unit
val loadable : t -> bool
val run : t -> unit
