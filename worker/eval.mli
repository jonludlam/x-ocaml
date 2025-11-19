val setup_toplevel : unit -> unit

val execute :
  id:int ->
  line_number:int ->
  output:(loc:int -> X_protocol.output list -> unit) ->
  ?filename:string ->
  string ->
  X_protocol.output list
