(** Widgets: parts of the page that code running in the worker draws and that
    send the reader's actions back to it.

    js_top_worker's [Widget] library sends a widget as a view (a tree of
    elements, attributes and event handlers) or as a managed widget, drawn by an
    adapter: JavaScript the worker sends once per kind. A widget goes after the
    cell whose code made it, after that cell's earlier widgets. Each update of a
    view patches the elements already on the page, so an input keeps its focus
    and whatever the reader is typing.

    Adapters and attributes run in the page itself: code in a cell can run
    JavaScript in the page with them, as it can put anything on the page. *)

type t

type send =
  widget_id:string ->
  handler_id:string ->
  event_type:string ->
  value:string option ->
  unit

val create : unit -> t

val set_send : t -> send -> unit
(** Where the reader's actions go. An input's handlers send its value, or
    ["true"] or ["false"] for a checkbox or radio button. *)

val update : t -> owner:Brr.El.t option -> string -> Jv.t -> unit
(** [update t ~owner id view] shows [view], the worker's JSON, as widget [id],
    placing it after [owner] if it is new. A view that cannot be decoded shows
    as an error in the widget's place. *)

val owner : t -> string -> Brr.El.t option
(** The cell that made widget [id]. *)

val config : t -> string -> string -> unit
(** A managed widget's new configuration. *)

val command : t -> string -> string -> string -> unit
(** [command t id cmd data] for a managed widget. *)

val register_adapter : t -> string -> string -> unit
(** [register_adapter t kind js]: [js] evaluates to an object with [create],
    [update], [command] and [destroy] methods. Managed widgets of that kind
    already shown start. *)

val clear : t -> string -> unit
(** Remove widget [id]. *)

val clear_owned : t -> Brr.El.t -> unit
(** Remove the widgets a cell made. *)

val clear_all : t -> unit
