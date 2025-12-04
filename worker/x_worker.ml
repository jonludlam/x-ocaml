module Merlin_worker = Worker

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
  Js_of_ocaml.Worker.set_onmessage @@ fun marshaled_message ->
  match X_protocol.req_of_bytes marshaled_message with
  | Merlin (id, action) ->
      respond (Merlin_response (id, Merlin_worker.on_message action))
  | Format_config conf -> Ocamlfmt.configure conf
  | Format (id, code) -> ignore (reformat ~id code : string)
  | Eval (id, line_number, code, filename) ->
      (* Check if this is spec content (type check request) *)
      if Eval.is_spec_content code then
        let result = Eval.execute_type_checks ~id ?filename code in
        respond (Top_response (id, result))
      else begin
        let code = reformat ~id code in
        let output ~loc out = respond (Top_response_at (id, loc, out)) in
        let result = Eval.execute ~output ~id ~line_number ?filename code in
        respond (Top_response (id, result))
      end
  | Setup warnings_config ->
      Eval.setup_toplevel ?warnings:warnings_config ();
      Option.iter Merlin_worker.set_warnings warnings_config
