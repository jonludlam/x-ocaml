type t

val init :
  id:int ->
  run_on:[ `Click | `Load | `Never ] ->
  ?filename:string ->
  ?extra_style:Jstr.t ->
  ?inline_style:Jstr.t ->
  ?merlin:bool ->
  ?highlight:Editor.highlight_spec list ->
  ?on_change:(unit -> unit) ->
  Client.t ->
  Webcomponent.t ->
  t

val id : t -> int
val string_id : t -> string option
val set_string_id : t -> string option -> unit
val get_source : t -> string
val set_source : t -> string -> unit
val filename : t -> string option
val is_run_ok : t -> bool
val set_type_check_pending : t -> unit
val is_type_check_pending : t -> bool
val is_type_check_done : t -> bool
val set_highlight : t -> Editor.highlight_spec list -> unit
val clear_output : t -> unit
val clear_spec_results : t -> unit
val set_spec_results : t -> X_protocol.output list -> unit
val set_spec_output : t -> X_protocol.output list -> has_tests:bool -> unit
val start_tests : t -> int -> unit
val add_test_output : t -> X_protocol.output list -> unit
val tests_pending : t -> int
val spec_passed : t -> bool
val add_message : t -> int -> X_protocol.output list -> unit
val completed_run : t -> X_protocol.output list -> unit
val completed_type_check : t -> unit
val set_prev : prev:t option -> t -> unit
val receive_merlin : t -> Protocol.answer -> unit
val loadable : t -> bool
val run : t -> unit
val run_directly : t -> unit
