(** OffscreenCanvas bindings for brr *)

type t
(** The type for OffscreenCanvas objects *)

val transfer_control : Brr_canvas.Canvas.t -> t
(** [transfer_control canvas] transfers control of the canvas to an OffscreenCanvas.
    The original canvas becomes blank and cannot be drawn to anymore. *)

val of_jv : Jv.t -> t
val to_jv : t -> Jv.t

val width : t -> int
val height : t -> int
val set_width : t -> int -> unit
val set_height : t -> int -> unit

val get_context_2d : t -> Jv.t option
(** Get the 2D rendering context. Returns None if the context cannot be created. *)
