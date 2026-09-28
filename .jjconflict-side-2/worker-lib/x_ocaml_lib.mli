(** What code running in a cell can do on the page besides print.

    Libraries build on this to put things on the page that the reader can
    use: the page halves of such libraries are custom elements, and a cell
    shows one as a {e display}. *)

val output_html : string -> unit
(** Show HTML under the phrase being run, with its other output, and fixed;
    for HTML that changes, or calls back, see {!html}. *)

(** {1 Displays}

    A display is an element on the page, shown in a box after the cell whose
    code made it (or whose callback did), until the cell runs again or the
    toplevel restarts. Its tag names a custom element, which receives [data]
    through its [data] property, now and at each {!update}: data for an
    element not yet defined waits until it is, so a library can load its
    element's script after it first shows one. doc/displays.md says how to
    write such an element. *)

type display

val display : ?on_remove:(unit -> unit) -> tag:string -> string -> display
(** [display ~tag data] shows [<tag>] with [data]. [on_remove] is called when
    the display goes because its cell runs again. *)

val update : display -> string -> unit
(** New data for the display, if it is still shown. Data sent while the cell
    is still running is shown as it arrives: a long computation can show
    its progress. *)

val html : ?on_remove:(unit -> unit) -> string -> display
(** A display of HTML, which {!update} replaces. Unlike {!output_html}, it is
    shown after the cell, not under its phrase. Elements in it with a
    [data-callback] attribute naming a {!callback} token call it: an input
    with its value (["true"] or ["false"] for a checkbox) as it changes,
    anything else when clicked, with its [data-payload] attribute.

    Each update replaces the HTML, and with it an input the reader is
    using: keep controls in a display of their own, which is not updated. *)

val remove : display -> unit

val live : display -> bool
(** Whether the display is still shown. *)

(** {1 Callbacks} *)

val callback : (string -> unit) -> int
(** [callback f] is a token for [f], which an element on the page calls by
    dispatching an [x-ocaml-callback] event with [detail]
    [{ token; payload }]: [f payload] runs as part of the cell that registered
    it, and so is forgotten when that cell runs again. *)

(** {1 Elements} *)

val require : tag:string -> src:string -> unit
(** [require ~tag ~src]: the element [<tag>] is defined by the script at
    [src], relative to the worker's own URL. The page loads it once, unless
    [<tag>] is already defined. *)

(** {1 Drawing from the worker} *)

val animate : (float -> unit) -> unit -> unit
(** [animate f] calls [f t] at each of the worker's animation frames, [t]
    being the time in milliseconds, as part of the cell that called it, until
    that cell runs again, [f] raises, or the function it returns is called.
    Frames do not come while a cell is running: the worker is busy. *)

(** A canvas on the page that code in the worker draws on directly: the page
    hands the canvas's drawing surface (an [OffscreenCanvas]) to the worker,
    and what is drawn on it shows on the page with no message per frame. *)
module Canvas : sig
  type t
  (** A canvas's 2D drawing context. *)

  val display :
    ?on_remove:(unit -> unit) ->
    ?on_click:(float -> float -> unit) ->
    width:int ->
    height:int ->
    (t -> unit) ->
    display
  (** [display ~width ~height ready] shows a [width] by [height] canvas, and
      calls [ready ctx] once the page has handed it over, which is after the
      cell has finished running. Draw on [ctx] then, or from {!animate}.
      [on_click x y] is called with where the reader clicked, in canvas
      pixels. *)

  val width : t -> int
  val height : t -> int
  val set_fill_style : t -> string -> unit
  val set_stroke_style : t -> string -> unit
  val set_line_width : t -> float -> unit
  val set_font : t -> string -> unit
  val set_global_alpha : t -> float -> unit
  val fill_rect : t -> float -> float -> float -> float -> unit
  val stroke_rect : t -> float -> float -> float -> float -> unit
  val clear_rect : t -> float -> float -> float -> float -> unit
  val begin_path : t -> unit
  val close_path : t -> unit
  val move_to : t -> float -> float -> unit
  val line_to : t -> float -> float -> unit
  val arc : t -> float -> float -> float -> float -> float -> unit
  val fill : t -> unit
  val stroke : t -> unit
  val fill_text : t -> string -> float -> float -> unit
  val save : t -> unit
  val restore : t -> unit
  val translate : t -> float -> float -> unit
  val rotate : t -> float -> unit
  val scale : t -> float -> float -> unit

  (** The rest of the 2D context, by name. *)

  type arg = Float of float | Int of int | String of string | Bool of bool

  val call : t -> string -> arg list -> unit
  (** [call ctx "ellipse" [ Float x; ... ]] calls a method of the context. *)

  val set : t -> string -> arg -> unit
  (** [set ctx "lineCap" (String "round")] sets a property of the context. *)
end

(**/**)

val callback_js : (string -> Js_of_ocaml.Js.Unsafe.any -> unit) -> int
val invoke_js : int -> string -> Js_of_ocaml.Js.Unsafe.any -> unit

val id : (int * int) ref
val forget_cell : int -> unit
val invoke : int -> string -> unit
