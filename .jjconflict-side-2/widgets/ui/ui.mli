(** Widgets for x-ocaml cells: controls, layouts, plots, images, timers and
    tables, in the spirit of Jupyter's ipywidgets.

    State lives in {!Var}s. A widget reads variables as it is drawn, and a
    widget on the page ({!show}) is drawn again whenever a variable it read
    changes, so the page follows the program without being told:

    {[
      let n = Ui.Var.create 10

      let () =
        Ui.show
          (Ui.vbox
             [ Ui.int_slider ~label:"n" ~max:100 n;
               Ui.dyn (fun () -> Ui.text (string_of_int (Ui.Var.get n * 2))) ])
    ]}

    Build the widgets once, outside {!dyn}: a widget made inside [dyn] is made
    afresh each time it is drawn, and loses any state of its own (a table's
    sorting, the selected tab). Everything a handler changes is drawn once
    when the handler returns.

    Widgets are views of js_top_worker's [Widget] library, which
    x-ocaml-widgets draws as displays under the cell that made them, patching
    the page in place: an input keeps its focus while the program redraws
    it. *)

(** {1 State} *)

module Var : sig
  type 'a t
  (** A variable: a value that widgets watch. *)

  val create : ?eq:('a -> 'a -> bool) -> 'a -> 'a t
  (** [eq] decides whether a new value is a change. By default it is [( = )],
      except that a value holding a function equals only itself. *)

  val get : 'a t -> 'a
  (** The value; a widget being drawn that reads it is drawn again when it
      changes. *)

  val peek : 'a t -> 'a
  (** The value, without watching it. *)

  val set : 'a t -> 'a -> unit
  val update : 'a t -> ('a -> 'a) -> unit

  val watch : 'a t -> ('a -> unit) -> unit
  (** [watch v f] calls [f] with each new value. *)

  val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
  (** A variable that follows another. *)

  val map2 :
    ?eq:('c -> 'c -> bool) -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val batch : (unit -> unit) -> unit
  (** [batch f] draws what [f] changed once, when it returns. Handlers are
      batched already. *)
end

val link : 'a Var.t -> 'a Var.t -> unit
(** Keep two variables equal, starting from the first's value. *)

val dlink : f:('a -> 'b) -> 'a Var.t -> 'b Var.t -> unit
(** [dlink ~f a b] sets [b] to [f a] now and whenever [a] changes. *)

(** {1 Widgets} *)

type t
(** A widget. *)

val show : ?id:string -> t -> unit
(** Put a widget on the page, under the cell running. Showing the same
    widget twice shows two copies that stay in step. [id] names it: showing
    another widget under the same [id] replaces it. *)

val dyn : (unit -> t) -> t
(** A widget computed as it is drawn, from the variables it reads. *)

(** {2 Text} *)

val text : string -> t
val title : string -> t
val note : string -> t
val pre : string -> t
val text_of : ('a -> string) -> 'a Var.t -> t
(** A variable, as text. *)

val badge : ?kind:string -> string -> t
(** A pill: [kind] is ["info"] (the default), ["good"], ["bad"] or ["warn"]. *)

(** {2 Numbers} *)

val int_slider :
  ?label:string -> ?min:int -> ?max:int -> ?step:int -> ?disabled:bool -> int Var.t -> t

val float_slider :
  ?label:string ->
  ?min:float ->
  ?max:float ->
  ?step:float ->
  ?decimals:int ->
  ?disabled:bool ->
  float Var.t ->
  t

val log_slider :
  ?label:string -> ?base:float -> ?min:float -> ?max:float -> ?step:float -> float Var.t -> t
(** A slider over exponents: [min] and [max] are powers of [base]. *)

val range_slider : ?label:string -> ?min:int -> ?max:int -> ?step:int -> (int * int) Var.t -> t
(** Two thumbs, for a low and a high value. *)

val int_input : ?label:string -> ?min:int -> ?max:int -> ?step:int -> int Var.t -> t
val float_input : ?label:string -> ?min:float -> ?max:float -> ?step:float -> float Var.t -> t

val progress : ?label:string -> ?max:float -> ?kind:string -> float Var.t -> t
(** A bar, full at [max] (1 by default); [kind] as for {!badge}, or
    ["accent"]. *)

(** {2 Yes or no} *)

val checkbox : ?label:string -> ?disabled:bool -> bool Var.t -> t
val switch : ?label:string -> bool Var.t -> t
val toggle_button : ?icon:string -> string -> bool Var.t -> t

val valid : ?yes:string -> ?no:string -> bool Var.t -> t
(** A tick or a cross. *)

(** {2 Choices}

    Options are pairs of a name and a value; a choice holds the value. Values
    that are functions are the same only if they are the same closure: give
    controls of one variable the same list of options (each mention of a
    primitive, such as [sin], makes a new closure). *)

val dropdown : ?label:string -> (string * 'a) list -> 'a Var.t -> t
val radio : ?label:string -> (string * 'a) list -> 'a Var.t -> t
val toggle_buttons : ?label:string -> (string * 'a) list -> 'a Var.t -> t

val select_multiple : ?label:string -> (string * 'a) list -> 'a list Var.t -> t
(** Chips to pick any number of; the list keeps the options' order. *)

val selection_slider : ?label:string -> (string * 'a) list -> 'a Var.t -> t

(** {2 Text} *)

val text_input :
  ?label:string -> ?placeholder:string -> ?on_submit:(string -> unit) -> string Var.t -> t
(** [on_submit] is called on Enter, or when the input loses focus changed. *)

val password : ?label:string -> ?placeholder:string -> string Var.t -> t
val textarea : ?label:string -> ?placeholder:string -> ?rows:int -> string Var.t -> t

val combobox :
  ?label:string -> ?placeholder:string -> options:string list -> string Var.t -> t
(** Free text, with suggestions. *)

val tags : ?label:string -> ?placeholder:string -> string list Var.t -> t
(** A list of words: type one and press Enter to add it; × removes it. *)

val color_picker : ?label:string -> string Var.t -> t
(** A colour as [#rrggbb]. *)

val date_picker : ?label:string -> string Var.t -> t
(** A date as [yyyy-mm-dd]. *)

val time_picker : ?label:string -> string Var.t -> t
(** A time as [hh:mm]. *)

(** {2 Actions and time} *)

val button :
  ?kind:string -> ?icon:string -> ?disabled:bool -> string -> (unit -> unit) -> t
(** [kind] is ["plain"] (the default), ["primary"], ["success"], ["warning"]
    or ["danger"]. *)

val clock : ?interval:int -> ?every:int Var.t -> running:bool Var.t -> (unit -> unit) -> t
(** Calls the function every [interval] milliseconds (100 by default), or
    every [every] as that variable changes, while [running] is true. A tick waits for the one before to be handled, so a
    slow function slows the clock rather than piling ticks up. It draws
    nothing: put it anywhere in a widget. *)

val play :
  ?label:string -> ?interval:int -> ?every:int Var.t -> ?loop:bool -> ?min:int -> max:int -> int Var.t -> t
(** Play, pause, stop and step buttons that count a variable from [min] to
    [max], and a slider for it. *)

(** {2 Events} *)

module Event : sig
  type t = {
    value : string option;  (** an input's value *)
    x : float;  (** the pointer, relative to the element *)
    y : float;
    width : float;  (** the element's size *)
    height : float;
    key : string;  (** a keyboard event's key *)
    shift : bool;
    alt : bool;
    ctrl : bool;
    meta : bool;
    buttons : int;  (** the mouse buttons held *)
    dx : float;  (** a wheel's movement *)
    dy : float;
  }
end

val keyboard : ?prevent:bool -> on_key:(Event.t -> unit) -> t -> t
(** A widget that hears keys pressed while it has focus (click it, or tab to
    it). With [prevent] (the default), keys do only what [on_key] does, and
    arrows do not scroll the page. *)

val element :
  ?cls:string list ->
  ?style:(string * string) list ->
  ?attrs:(string * string) list ->
  ?on:(string * (Event.t -> unit)) list ->
  string ->
  t list ->
  t
(** Any element: [element "h3" [ text "hello" ]]. SVG elements get SVG's
    namespace. *)

val node : Widget.View.node -> t
(** A view of js_top_worker's widget library, as it is. *)

(** {2 Output} *)

module Output : sig
  type t

  val create : ?limit:int -> unit -> t
  (** Keeps the last [limit] lines (200 by default). *)

  val print : t -> string -> unit
  val printf : t -> ('a, unit, string, unit) format4 -> 'a
  val clear : t -> unit
end

val output : ?height:string -> Output.t -> t
(** Where an {!Output.t}'s lines show; handlers can print to it. *)

(** {2 Layout} *)

val hbox : ?gap:string -> ?align:string -> ?wrap:bool -> t list -> t
val vbox : ?gap:string -> t list -> t
val grid : ?gap:string -> ?columns:int -> t list -> t
val card : ?title:string -> t list -> t

val tabs : (string * t) list -> t
(** Only the chosen tab's widget is drawn. *)

val accordion : ?open_:int list -> (string * t) list -> t
(** Sections that open and close; [open_] are open to start with. *)

val styled : (string * string) list -> t -> t
(** A widget in a box with these CSS properties. *)

(** {2 Tables} *)

val table : ?page_size:int -> header:string list -> (unit -> string list list) -> t
(** Rows of cells, with a filter, sorting by any column (click its heading:
    numbers sort as numbers) and pages. The rows are asked for each time the
    table is drawn, so they can come from variables. *)

(** {2 Images} *)

module Image : sig
  type t

  val rgb : width:int -> height:int -> (int -> int -> int * int * int) -> t
  (** [rgb ~width ~height f]: the colour of pixel [(x, y)] is [f x y], its
      red, green and blue from 0 to 255. *)

  val of_url : width:int -> height:int -> string -> t
  val hsv : float -> float -> float -> int * int * int
  (** [hsv h s v]: [h] in turns, [s] and [v] from 0 to 1, as red, green and
      blue. *)
end

val image :
  ?scale:float -> ?smooth:bool -> ?on_click:(shift:bool -> int -> int -> unit) -> ?alt:string -> Image.t -> t
(** Pixels are drawn as squares unless [smooth]. [on_click] receives the
    pixel clicked. *)

(** {2 Plots} *)

module Plot : sig
  type series

  val line : ?label:string -> ?color:string -> ?width:float -> ?dash:bool -> (float * float) list -> series
  (** Points joined up; it breaks where a point is not finite. *)

  val area : ?label:string -> ?color:string -> (float * float) list -> series
  val scatter : ?label:string -> ?color:string -> ?size:float -> ?tips:string list -> (float * float) list -> series
  (** [tips] are what hovering over each point says. *)

  val bars : ?label:string -> ?color:string -> ?width:float -> (float * float) list -> series
  (** Bars centred on each [x]. *)

  val histogram : ?label:string -> ?color:string -> ?bins:int -> float list -> series
  val fn : ?label:string -> ?color:string -> ?samples:int -> (float -> float) -> series
  (** A function, across the chart's width. *)

  val hline : ?color:string -> float -> series
  val vline : ?color:string -> float -> series
  val text : ?color:string -> float * float -> string -> series

  val palette : string array
  (** The colours series take in turn. *)

  type pointer = { x : float; y : float; shift : bool; alt : bool; buttons : int }
  (** Where the pointer is, in the chart's own coordinates. *)

  val chart :
    ?title:string ->
    ?x_label:string ->
    ?y_label:string ->
    ?width:int ->
    ?height:int ->
    ?x_range:float * float ->
    ?y_range:float * float ->
    ?legend:bool ->
    ?grid:bool ->
    ?on_click:(pointer -> unit) ->
    ?on_press:(pointer -> unit) ->
    ?on_drag:(pointer -> unit) ->
    ?on_release:(pointer -> unit) ->
    ?on_move:(pointer -> unit) ->
    ?aspect_equal:bool ->
    series list ->
    t
  (** An SVG chart, [width] by [height] units (640 by 360), as wide as there
      is room for. The ranges fit the data unless given. Pointer handlers
      hear the plot area: [on_press] when a button goes down, [on_drag] as
      the pointer moves with it held, [on_release] when it comes up (over
      the chart, anywhere), [on_move] as it moves with none held.
      [aspect_equal] makes a unit the same length on both axes. *)

  val bar_chart :
    ?title:string ->
    ?horizontal:bool ->
    ?color:string ->
    ?width:int ->
    ?height:int ->
    ?on_click:(int -> unit) ->
    ?format:(float -> string) ->
    (string * float) list ->
    t
  (** Named bars; [on_click] receives the bar's index. *)

  val pie : ?title:string -> ?donut:bool -> ?size:int -> ?on_click:(int -> unit) -> (string * float) list -> t
end

(** {2 Interact}

    Controls from a description, and a widget computed from their values,
    drawn again as they change, like ipywidgets' [interact]:

    {[
      open Ui

      let () =
        show
          (interact
             (let+ f = Interact.float "frequency" ~max:10. 3.
              and+ c = Interact.color "colour" "#e8661d" in
              Plot.(chart [ fn ~color:c (fun x -> sin (f *. x)) ])))
    ]}

    [let+] and [and+] are also {!Ui}'s own, so the body after [in] is not
    inside [Interact], whose [int], [float] and [text] would hide the usual
    ones. *)

module Interact : sig
  type 'a control

  val int : ?min:int -> ?max:int -> ?step:int -> string -> int -> int control
  val float : ?min:float -> ?max:float -> ?step:float -> string -> float -> float control
  val log : ?min:float -> ?max:float -> string -> float -> float control
  val bool : string -> bool -> bool control
  val choice : string -> (string * 'a) list -> 'a control
  val buttons : string -> (string * 'a) list -> 'a control
  val text : string -> string -> string control
  val color : string -> string -> string control
  val range : ?min:int -> ?max:int -> string -> int * int -> (int * int) control

  val var : t -> 'a Var.t -> 'a control
  (** Any widget, and the variable it sets. *)

  val const : 'a -> 'a control
  val map : ('a -> 'b) -> 'a control -> 'b control
  val both : 'a control -> 'b control -> ('a * 'b) control
  val ( let+ ) : 'a control -> ('a -> 'b) -> 'b control
  val ( and+ ) : 'a control -> 'b control -> ('a * 'b) control
end

val ( let+ ) : 'a Interact.control -> ('a -> 'b) -> 'b Interact.control
val ( and+ ) : 'a Interact.control -> 'b Interact.control -> ('a * 'b) Interact.control

val interact : ?manual:bool -> ?side:bool -> t Interact.control -> t
(** The controls, and the widget from their values. With [manual], the
    widget is computed only when Run is pressed; with [side], the controls
    are beside it rather than above. *)
