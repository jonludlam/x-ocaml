(* Stub: js_top_worker backend removed to avoid dependency *)
type t
val make : string -> t
val on_message : t -> (X_protocol.response -> unit) -> unit
val init : t -> unit
val post : t -> X_protocol.request -> unit
val eval : id:int -> line_number:int -> t -> string -> unit
val fmt : id:int -> t -> string -> unit
