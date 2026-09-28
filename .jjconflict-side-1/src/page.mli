(** The cells of a page and the worker they share.

    The page owns the worker: it starts it, sends it the page-wide
    configuration, and routes each of its answers to the cell it is for. Cells
    are registered in document order, each running after the one before. *)

type t

val create :
  worker:Client.t ->
  worker_url:string ->
  ?extra_style:Jstr.t ->
  ?inline_style:Jstr.t ->
  ?default_run_on:string ->
  ?format_config:string ->
  unit ->
  t
(** [create ~worker ~worker_url ()] sets the worker up for the page. Scripts
    that define displays' elements are relative to [worker_url].
    [default_run_on] (["load"] or ["click"]) applies to cells without a [run-on]
    attribute; it defaults to ["load"]. *)

val register : t -> Webcomponent.t -> Cell.t
(** [register page element] makes a cell of a newly connected [<x-ocaml>]
    element, after the cells registered before it, and runs it if every cell so
    far runs on load. *)
