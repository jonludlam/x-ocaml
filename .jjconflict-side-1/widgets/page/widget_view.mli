(** Views of js_top_worker's [Widget] library, drawn and patched in place.

    A view is a tree of elements, attributes and named handlers. Each update
    patches the elements already on the page rather than replacing them, so
    an input keeps its focus and what the reader is typing. Where a view gives
    an input's value or checked state, the input shows it at every update,
    except for values the input itself sent since the view last agreed with
    it: a program echoing an input lags behind the reader.

    Events say what happened. An input's handlers send its value (["true"] or
    ["false"] for a checkbox or radio button). Keyboard events send JSON with
    [key], [code], [repeat], the modifiers and an input's [value]; mouse,
    pointer, touch and wheel events send [x] and [y] relative to the element
    (in the user units of its [<svg>] for SVG), its [width] and [height],
    [button], [buttons], the modifiers, and [dx] and [dy] for the wheel.
    Moves, wheel and scroll events go at most once a frame. An element's
    [data-x-ocaml-prevent] attribute lists events whose default its handlers
    prevent.

    A managed widget, inside a view or the whole of one, is drawn by an
    adapter registered for its kind. *)

type send =
  widget_id:string ->
  handler_id:string ->
  event_type:string ->
  value:string option ->
  unit

type node
type rendered
type managed
type adapter

type ctx = {
  widget_id : string;
  send : send;
  adapters : (string, adapter) Hashtbl.t;
}

val decode : Jv.t -> (node, string) result
(** A view, from the JSON js_top_worker's widget library makes. *)

val render : ?svg:bool -> ctx -> node -> rendered
val patch : ctx -> rendered -> node -> rendered
val drop : ctx -> rendered -> unit
(** Stop listening, and destroy managed widgets. *)

val dom : rendered -> Brr.El.t
val placeholder : string -> Brr.El.t
(** An error message in a widget's place. *)

val adapters : (string, adapter) Hashtbl.t

val register_adapter : string -> string -> unit
(** [register_adapter kind js]: [js] evaluates to an object with [create],
    [update], [command] and [destroy] methods. *)

val waiting : string -> rendered -> managed list
(** The managed widgets of that kind with no adapter yet. *)

val start : ctx -> managed -> unit

val command : ctx -> rendered -> string -> string -> unit
(** [command ctx r cmd data] for [r], if it is a managed widget. *)
