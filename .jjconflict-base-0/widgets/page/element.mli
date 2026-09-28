(** Custom elements for x-ocaml's displays (see X_ocaml_lib). *)

val define :
  string -> on_data:(Brr.El.t -> string -> unit) -> on_disconnect:(Brr.El.t -> unit) -> unit
(** [define tag ~on_data ~on_disconnect] defines [<tag>]: [on_data] receives
    each display's data, [on_disconnect] its leaving the page. *)

val callback : Brr.El.t -> token:int -> string -> unit
(** [callback el ~token payload] calls the worker's callback [token]. *)
