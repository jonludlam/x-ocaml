module X_canvas = X_canvas
module X_context = X_context

let id = X_context.current_id

let output_html m =
  let id, loc = !id in
  Js_of_ocaml.Worker.post_message
    (X_protocol.resp_to_bytes
       (X_protocol.Top_response_at (id, loc, [ Html m ])));
  ()
