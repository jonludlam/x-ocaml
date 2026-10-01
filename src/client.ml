module Worker = Brr_webworkers.Worker
open Brr

(* The worker is started from a data: URL, where relative URLs mean
   nothing, so resolve them against the page as the browser would. *)
let absolute_url url =
  let base = Jv.get (Document.to_jv G.document) "baseURI" in
  let url = Jv.new' (Jv.get Jv.global "URL") [| Jv.of_string url; base |] in
  Jv.to_string (Jv.get url "href")

let wrap_url ?extra_load url =
  let url = absolute_url url in
  let extra =
    match extra_load with
    | None -> ""
    | Some extra -> "','" ^ absolute_url extra
  in
  let script = "importScripts('" ^ url ^ extra ^ "');" in
  let script = Jstr.of_string script in
  let url =
    match Base64.(encode (data_of_binary_jstr script)) with
    | Ok data -> Jstr.to_string data
    | Error _ -> assert false
  in
  "data:text/javascript;base64," ^ url

type t = {
  script : Jstr.t; (* the data: URL the worker starts from *)
  mutable worker : Worker.t;
  mutable handler : X_protocol.response -> unit;
}

let listen t =
  let fn m =
    let m = Ev.as_type m in
    let msg = Bytes.to_string @@ Brr_io.Message.Ev.data m in
    t.handler (X_protocol.resp_of_string msg)
  in
  ignore (Ev.listen Brr_io.Message.Ev.message fn @@ Worker.as_target t.worker)

let make ?extra_load url =
  let script = Jstr.of_string @@ wrap_url ?extra_load url in
  let t = { script; worker = Worker.create script; handler = ignore } in
  listen t;
  t

let on_message t fn = t.handler <- fn
let post t msg = Worker.post t.worker (X_protocol.req_to_bytes msg)

let restart t =
  Worker.terminate t.worker;
  t.worker <- Worker.create t.script;
  listen t

let eval ?(verbatim = false) ~id ~line_number t code =
  post t
    (if verbatim then Eval_verbatim (id, line_number, code)
     else Eval (id, line_number, code))
let fmt ~id t code = post t (Format (id, code))
