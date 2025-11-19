type t

val make : ?extra_load:string -> string -> t
val on_message : t -> (X_protocol.response -> unit) -> unit
val post : t -> X_protocol.request -> unit
val eval : id:int -> line_number:int -> ?filename:string -> t -> string -> unit
val fmt : id:int -> t -> string -> unit
