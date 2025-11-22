type t

val init :
  id:int ->
  run_on:[ `Click | `Load | `Never ] ->
  ?filename:string ->
  ?extra_style:Jstr.t ->
  ?inline_style:Jstr.t ->
  ?merlin:bool ->
  ?highlight:Editor.highlight_spec list ->
  Client.t ->
  Webcomponent.t ->
  t

val id : t -> int
val get_source : t -> string
val set_source : t -> string -> unit
val set_highlight : t -> Editor.highlight_spec list -> unit
val clear_output : t -> unit
val add_message : t -> int -> X_protocol.output list -> unit
val completed_run : t -> X_protocol.output list -> unit
val set_prev : prev:t option -> t -> unit
val receive_merlin : t -> Protocol.answer -> unit
val loadable : t -> bool
val run : t -> unit
