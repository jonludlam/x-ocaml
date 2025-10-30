(** Canvas widget API for x-ocaml *)

type t
(** Canvas handle *)

(** {1 Events} *)

type mouse_event = { x : int; y : int; button : int }
(** Mouse event data *)

type event =
  | Mouse_down of mouse_event
  | Mouse_move of { x : int; y : int }
  | Mouse_up of mouse_event  (** Canvas events *)

(** {1 Creation} *)

val create : width:int -> height:int -> int
(** [create ~width ~height] creates a new canvas widget and returns its ID. The
    canvas will be displayed in the notebook output. Note: The canvas is not
    immediately available; use {!get} to check when ready. *)

val get : int -> t option
(** [get widget_id] returns the canvas handle by ID. Returns [None] until the
    canvas is initialized by the frontend. *)

val when_ready : int -> (t -> unit) -> unit
(** [when_ready widget_id callback] calls [callback] when the canvas is ready.
    If the canvas is already ready, calls immediately. Otherwise, registers the
    callback to be called when the canvas is initialized. *)

(** {1 Event Handling} *)

val on_event : int -> (t -> event -> unit) -> unit
(** [on_event widget_id handler] registers an event handler for the canvas
    widget. *)

val dispatch_event : int -> X_protocol.widget_event -> unit
(** Internal: dispatch event to registered handler *)

val register_offscreen : int -> Jv.t -> t
(** Internal: called when OffscreenCanvas is transferred from frontend *)

(** {1 Drawing API} *)

val set_fill_style : t -> string -> unit
(** Set the fill color (e.g., "red", "#ff0000", "rgb(255,0,0)") *)

val set_stroke_style : t -> string -> unit
(** Set the stroke color *)

val set_line_width : t -> float -> unit
(** Set the line width for strokes *)

val fill_rect : t -> x:int -> y:int -> w:int -> h:int -> unit
(** Draw a filled rectangle *)

val stroke_rect : t -> x:int -> y:int -> w:int -> h:int -> unit
(** Draw a rectangle outline *)

val clear_rect : t -> x:int -> y:int -> w:int -> h:int -> unit
(** Clear a rectangular area *)

val begin_path : t -> unit
(** Begin a new path *)

val move_to : t -> x:int -> y:int -> unit
(** Move the path to a point *)

val line_to : t -> x:int -> y:int -> unit
(** Add a line to the path *)

val arc : t -> x:int -> y:int -> r:int -> start:float -> end_:float -> unit
(** Add an arc to the path *)

val fill : t -> unit
(** Fill the current path *)

val stroke : t -> unit
(** Stroke the current path *)

val get_context : t -> Jv.t
(** Get the raw 2D context for advanced usage *)

val get_offscreen : t -> Jv.t
(** Get the raw OffscreenCanvas object for WebGL or advanced usage *)
