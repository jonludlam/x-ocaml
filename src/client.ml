module Worker = Brr_webworkers.Worker
open Brr

type t = Worker.t

let current_url =
  let url = Window.location G.window in
  let path = Jstr.to_string (Uri.path url) in
  let url =
    match List.rev (String.split_on_char '/' path) with
    | [] | "" :: _ -> url
    | _ :: rev_path -> (
        let path = Jstr.of_string @@ String.concat "/" @@ List.rev rev_path in
        match Uri.with_uri ~path ~query:Jstr.empty ~fragment:Jstr.empty url with
        | Ok url -> url
        | Error _ -> url)
  in
  Jstr.to_string (Uri.to_jstr url)

let absolute_url url =
  (* If URL is already absolute, return it as-is *)
  if String.starts_with ~prefix:"http:" url
      || String.starts_with ~prefix:"https:" url
      || String.starts_with ~prefix:"file:" url
      || String.starts_with ~prefix:"/" url
      || String.starts_with ~prefix:"data:" url
  then url
  (* If current URL is about:srcdoc or similar, can't make relative URLs absolute *)
  else if String.starts_with ~prefix:"about:" current_url
  then url  (* Return as-is, hope it works or is handled elsewhere *)
  else
    (* Strip leading ./ if present *)
    let url = if String.starts_with ~prefix:"./" url then String.sub url 2 (String.length url - 2) else url in
    (* Ensure there's a separator between current_url and the relative path *)
    let separator = if String.ends_with ~suffix:"/" current_url then "" else "/" in
    current_url ^ separator ^ url

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

let make ?extra_load url =
  Worker.create @@ Jstr.of_string @@ wrap_url ?extra_load url

let on_message t fn =
  let fn m =
    let m = Ev.as_type m in
    let msg = Bytes.to_string @@ Brr_io.Message.Ev.data m in
    fn (X_protocol.resp_of_string msg)
  in
  let _listener =
    Ev.listen Brr_io.Message.Ev.message fn @@ Worker.as_target t
  in
  ()

let post worker msg = Worker.post worker (X_protocol.req_to_bytes msg)

let eval ~id ~line_number ?filename worker code =
  post worker (Eval (id, line_number, code, filename))

let fmt ~id worker code = post worker (Format (id, code))
