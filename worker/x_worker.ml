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

(* The one message that is not marshalled: a callback that brings a JS value
   the page transferred, an OffscreenCanvas say, which cannot be marshalled.
   It is [{ xOcamlTransfer: token, payload, value }]. *)
let transfer message =
  let open Js_of_ocaml.Js in
  let token = Unsafe.get message "xOcamlTransfer" in
  if Unsafe.equals token undefined then None
  else
    Some
      ( (token : int),
        to_string (Unsafe.get message "payload"),
        (Unsafe.get message "value" : Unsafe.any) )

let handle marshaled_message =
  match X_protocol.req_of_bytes marshaled_message with
  | Merlin (id, action) ->
      respond (Merlin_response (id, Merlin_worker.on_message action))
  | Format_config conf -> Ocamlfmt.configure conf
  | Format (id, code) -> ignore (reformat ~id code : string)
  | Callback (k, payload) -> (
      try X_ocaml_cells.invoke k payload
      with e ->
        Brr.Console.error [ "x-ocaml: a callback raised"; Printexc.to_string e ]
      )
  | Eval (id, line_number, code) ->
      (* What the cell showed or registered when it last ran goes. *)
      X_ocaml_cells.forget_cell id;
      let code = reformat ~id code in
      let output ~loc out = respond (Top_response_at (id, loc, out)) in
      let result = Eval.execute ~output ~id ~line_number code in
      respond (Top_response (id, result))
  | Setup -> Eval.setup_toplevel ()

let run () =
  Js_of_ocaml.Worker.set_onmessage @@ fun message ->
  match transfer message with
  | Some (k, payload, v) -> (
      try X_ocaml_lib.invoke_js k payload v
      with e ->
        Brr.Console.error [ "x-ocaml: a callback raised"; Printexc.to_string e ]
      )
  | None -> handle message
