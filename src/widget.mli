open Brr

(** Widget management for x-ocaml *)

type kind =
  | Canvas of { canvas : El.t; offscreen : Offscreen_canvas.t }
      (** Widget types *)

type t
(** Widget handle *)

val create :
  cell_id:int -> widget_id:int -> Client.t -> X_protocol.widget_kind -> t
(** Create a widget in the DOM and set up communication with worker *)

val to_el : t -> El.t
(** Get the container element for the widget *)
