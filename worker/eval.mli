val setup_toplevel : ?warnings:string -> unit -> unit

val execute :
  id:int ->
  line_number:int ->
  output:(loc:int -> X_protocol.output list -> unit) ->
  ?filename:string ->
  string ->
  X_protocol.output list

val is_spec_content : string -> bool
val execute_type_checks : id:int -> ?filename:string -> string -> X_protocol.output list
