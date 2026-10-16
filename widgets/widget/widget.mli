(** Interactive widgets in x-ocaml cells, drawn as displays.

    A widget is a view, a tree of elements whose event handlers are named, and
    the OCaml functions those names stand for. It is shown as a display (see
    {!X_ocaml_lib}) of an [<x-ocaml-view>] element, which the page half,
    [x-ocaml-widgets.js], defines; the page loads it from beside the worker
    when the first widget is shown. When the reader uses the widget, the
    element calls the handler named, with the input's value if there is one.

    Like any display, a widget belongs to the cell that showed it, and goes
    when that cell runs again: then its handler ["x-ocaml:removed"], if it has
    one, is called, so that whatever was updating the widget can stop.

    {[
      let count = ref 0

      let rec view () =
        let open Widget.View in
        Element
          {
            tag = "button";
            attrs = [ Handler ("click", "add") ];
            children = [ Text (string_of_int !count) ];
          }

      let () =
        Widget.display ~id:"counter"
          ~handlers:
            [ ("add", fun _ -> incr count; Widget.update ~id:"counter" (view ())) ]
          (view ())
    ]} *)

module View : sig
  type event_id = string

  type attr =
    | Property of string * string
    | Style of string * string
    | Class of string
    | Handler of string * event_id  (** An event's name, and its handler's. *)

  type node =
    | Text of string
    | Element of { tag : string; attrs : attr list; children : node list }
    | Managed of { kind : string; config : string }
        (** Drawn by the adapter registered for [kind] (see
            {!register_adapter}), given [config], a JSON string. *)
end

val display :
  id:string ->
  handlers:(string * (string option -> unit)) list ->
  View.node ->
  unit
(** [display ~id ~handlers view] shows a widget, or, if there is one called
    [id], gives it this view and these handlers. *)

val update : id:string -> View.node -> unit
(** A new view for a widget; its handlers stay. *)

val clear : id:string -> unit
(** Removes a widget. *)

val display_managed :
  id:string ->
  kind:string ->
  config:string ->
  handlers:(string * (string option -> unit)) list ->
  unit
(** [display ~id ~handlers (Managed { kind; config })]. *)

val update_config : id:string -> string -> unit
(** A new configuration for a managed widget, which its adapter reconciles
    with what it shows. *)

val command : id:string -> string -> string -> unit
(** [command ~id name data] has a managed widget's adapter do something once,
    such as an animation; [data] is JSON. *)

val register_adapter : kind:string -> js:string -> unit
(** [register_adapter ~kind ~js] defines how managed widgets of a kind are
    drawn. [js] is an expression whose value has the methods
    [create(container, config, send)], returning the adapter's state;
    [update(state, config)]; [command(state, name, data)]; and
    [destroy(state)]. [send(handler, value)] calls a handler. *)
