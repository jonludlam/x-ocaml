(** A reader's version of an exercise, kept in the browser's local storage under
    the page's path and the exercise's [data-id], so it survives a reload.
    Storage may be unavailable (a private window, blocked site data); then
    nothing is kept and nothing is found. *)

val find : string -> string option
(** [find id] is the version kept for the exercise [id] on this page. *)

val keep : string -> string -> unit
(** [keep id source] keeps [source] as the version of exercise [id]. *)

val forget : string -> unit
