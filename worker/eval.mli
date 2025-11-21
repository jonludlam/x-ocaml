val setup_toplevel : ?warnings:string -> unit -> unit

val execute :
  id:int ->
  line_number:int ->
  output:(loc:int -> X_protocol.output list -> unit) ->
  ?filename:string ->
  string ->
  X_protocol.output list
