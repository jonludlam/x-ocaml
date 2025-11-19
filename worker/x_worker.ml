module Merlin_worker = Worker

(* Ensure PPX registration happens by loading the module *)
module _ = Register_js_ppx

let respond m = Js_of_ocaml.Worker.post_message (X_protocol.resp_to_bytes m)

let reformat ~id code =
  let code' =
    try Ocamlfmt.fmt code
    with err ->
      Brr.Console.error [ "OCamlformat error:"; Printexc.to_string err ];
      code
  in
  if code <> code' then respond (Formatted_source (id, code'));
  code'

let run () =
  Brr.Console.log
    [
      "Worker loaded - VERSION " ^ Version.version
      ^ " with X_canvas and WebGL support";
    ];
  (* Use Brr's event listener to get raw MessageEvent *)
  let handle_message ev =
    let msg_ev = Brr.Ev.as_type ev in
    let data = Brr_io.Message.Ev.data msg_ev in

    (* Try to decode as marshaled bytes first *)
    try
      match X_protocol.req_of_bytes data with
      | Merlin (id, action) ->
          respond (Merlin_response (id, Merlin_worker.on_message action))
      | Format_config conf -> Ocamlfmt.configure conf
      | Format (id, code) -> ignore (reformat ~id code : string)
      | Eval (id, line_number, code) ->
          let code = reformat ~id code in
          let output ~loc out = respond (Top_response_at (id, loc, out)) in
          let result = Eval.execute ~output ~id ~line_number code in
          respond (Top_response (id, result))
      | Setup -> Eval.setup_toplevel ()
      | Widget_event (_cell_id, widget_id, event) ->
          X_ocaml_lib.X_canvas.dispatch_event widget_id event
    with _ -> (
      (* Not a marshaled message, try as raw JavaScript object *)
      let obj = Jv.repr data in
      Brr.Console.log [ "Received non-marshaled message:"; obj ];
      try
        let msg_type = Jv.to_jstr (Jv.get obj "type") in
        Brr.Console.log [ "Message type:"; msg_type ];
        if Jstr.equal msg_type (Jstr.v "offscreen_canvas") then
          let widget_id = Jv.Int.get obj "widget_id" in
          let canvas = Jv.get obj "canvas" in
          Brr.Console.log [ "Registering canvas with widget_id:"; Jv.of_int widget_id ];
          let _ = X_ocaml_lib.X_canvas.register_offscreen widget_id canvas in
          ()
      with err ->
        Brr.Console.error
          [ "Error processing widget message:"; Printexc.to_string err; "Message data:"; obj ])
  in
  let _listener =
    Brr.Ev.listen Brr_io.Message.Ev.message handle_message Brr.G.target
  in
  ()
