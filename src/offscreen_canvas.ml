open Brr

(* Type aliases for mli compatibility *)
type canvas = Brr_canvas.Canvas.t
type jv = Jv.t

(* OffscreenCanvas type *)
type t = Jv.t

(* Transfer control from HTMLCanvasElement *)
let transfer_control canvas =
  Jv.call (Brr_canvas.Canvas.to_jv canvas) "transferControlToOffscreen" [||]

(* Conversions *)
let of_jv jv = jv
let to_jv t = t

(* Canvas dimensions *)
let width t = Jv.Int.get t "width"
let height t = Jv.Int.get t "height"
let set_width t w = Jv.Int.set t "width" w
let set_height t h = Jv.Int.set t "height" h

(* Get 2D rendering context *)
let get_context_2d t =
  let ctx = Jv.call t "getContext" [| Jv.of_string "2d" |] in
  if Jv.is_null ctx then None else Some ctx
