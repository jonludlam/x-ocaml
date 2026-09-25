module Jtw = Js_top_worker_client_msg
module Msg = Js_top_worker_message.Message

type t = {
  url : string;
  mutable client : Jtw.t;
  mutable config : Msg.init_config option;
  mutable handler : X_protocol.response -> unit;
  widgets : Widget_render.t;
  mutable running : (int * Brr.El.t option) list;
      (* the cells sent to the worker and not finished, oldest first: the
         worker runs them in turn, so the first is the one running *)
  mutable last_owner : Brr.El.t option;
      (* where a widget made outside any cell's run goes: a widget event's
         handler runs after the cells *)
}

(* A worker on another origin cannot be started by URL; a same-origin blob
   that imports it can, and tells it where to find its files. *)
let worker_url url =
  let origin u = Jv.get (Jv.new' (Jv.get Jv.global "URL") [| Jv.of_string u; Jv.get (Brr.Document.to_jv Brr.G.document) "baseURI" |]) "origin" in
  let page = Jv.get (Brr.Window.to_jv Brr.G.window) "origin" in
  if Jv.to_string (origin url) = Jv.to_string page then url
  else
    let dir = match String.rindex_opt url '/' with Some i -> String.sub url 0 i | None -> url in
    let code =
      Printf.sprintf {|globalThis.__global_rel_url="%s";importScripts("%s");|} dir url
    in
    let blob =
      Jv.new' (Jv.get Jv.global "Blob")
        [| Jv.of_jv_array [| Jv.of_string code |];
           Jv.obj [| ("type", Jv.of_string "application/javascript") |] |]
    in
    Jv.to_string (Jv.call (Jv.get Jv.global "URL") "createObjectURL" [| blob |])

let owner t = match t.running with (_, o) :: _ -> o | [] -> t.last_owner

let wire t =
  let c = t.client in
  Jtw.set_on_widget_update c (fun id view ->
      Widget_render.update t.widgets ~owner:(owner t) id (Jv.repr view));
  Jtw.set_on_widget_clear c (Widget_render.clear t.widgets);
  Jtw.set_on_widget_config c (Widget_render.config t.widgets);
  Jtw.set_on_widget_command c (Widget_render.command t.widgets);
  Jtw.set_on_widget_register_adapter c (Widget_render.register_adapter t.widgets)

let make url =
  let t =
    { url; client = Jtw.create (worker_url url); config = None; handler = ignore;
      widgets = Widget_render.create (); running = []; last_owner = None }
  in
  Widget_render.set_send t.widgets (fun ~widget_id ~handler_id ~event_type ~value ->
      (* Widgets the handler makes go with the one the reader used. *)
      (match Widget_render.owner t.widgets widget_id with
       | Some _ as o -> t.last_owner <- o
       | None -> ());
      Jtw.send_widget_event t.client ~widget_id ~handler_id ~event_type ~value);
  wire t;
  t

let start t =
  Option.iter (fun config -> Lwt.async (fun () -> Jtw.init t.client config)) t.config

let init ?(findlib_requires = []) ?findlib_index t =
  (* The stdlib's cmis sit beside the index, in the universe. *)
  let stdlib_dcs =
    Option.bind findlib_index (fun fi ->
        Option.map
          (fun i -> String.sub fi 0 (i + 1) ^ "lib/ocaml/dynamic_cmis.json")
          (String.rindex_opt fi '/'))
  in
  t.config <- Some { Msg.findlib_requires; stdlib_dcs; findlib_index };
  start t

(* The widgets' handlers were in the old worker: nothing would answer them. *)
let restart t =
  Jtw.terminate t.client;
  Widget_render.clear_all t.widgets;
  t.running <- [];
  t.client <- Jtw.create (worker_url t.url);
  wire t;
  start t

let on_message t f = t.handler <- f
let respond t r = t.handler r

(* Merlin's answers come back in js_top_worker's own types. *)

let completion_kind = function
  | "Constructor" -> `Constructor
  | "Keyword" -> `Keyword
  | "Label" -> `Label
  | "MethodCall" -> `MethodCall
  | "Modtype" -> `Modtype
  | "Module" -> `Module
  | "Type" -> `Type
  | "Variant" -> `Variant
  | _ -> `Value

let completions (c : Msg.completions) : Protocol.completions =
  {
    Protocol.from = c.from;
    to_ = c.to_;
    entries =
      List.map
        (fun (e : Msg.compl_entry) ->
          {
            Query_protocol.Compl.name = e.name;
            kind = completion_kind e.kind;
            desc = e.desc;
            info = e.info;
            deprecated = e.deprecated;
#if defined OXCAML
            ppx_template_generated = false;
#endif
          })
        c.entries;
  }

let location (l : Msg.location) : Ocaml_parsing.Location.t =
  let pos (p : Msg.position) =
    { Lexing.pos_fname = ""; pos_lnum = p.pos_lnum; pos_bol = p.pos_bol; pos_cnum = p.pos_cnum }
  in
  { loc_start = pos l.loc_start; loc_end = pos l.loc_end; loc_ghost = false }

let after prefix s =
  let n = String.length prefix in
  if String.length s > n && String.sub s 0 n = prefix then
    Some (String.sub s n (String.length s - n))
  else None

let error (e : Msg.error) : Protocol.error =
  let kind : Ocaml_parsing.Location.report_kind =
    match (after "warning:" e.kind, after "warning_as_error:" e.kind,
           after "alert:" e.kind, after "alert_as_error:" e.kind) with
    | Some w, _, _, _ -> Report_warning w
    | _, Some w, _, _ -> Report_warning_as_error w
    | _, _, Some a, _ -> Report_alert a
    | _, _, _, Some a -> Report_alert_as_error a
    | _ -> Report_error
  in
  let source : Ocaml_parsing.Location.error_source =
    match e.source with
    | "lexer" -> Lexer
    | "parser" -> Parser
    | "typer" -> Typer
    | "warning" -> Warning
    | "env" -> Env
    | "config" -> Config
    | _ -> Unknown
  in
  { Protocol.kind; loc = location e.loc; main = e.main; sub = e.sub; source }

let type_info (t : Msg.type_info) =
  let tail =
    match t.tail with
    | "tail_position" -> `Tail_position
    | "tail_call" -> `Tail_call
    | _ -> `No
  in
  (location t.loc, `String t.type_str, tail)

let offset = function `Offset n -> n | _ -> 0

(* Run [f], answering [fallback] if it fails. *)
let answer t f fallback =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Lwt.map (respond t) (f ()))
        (fun _ -> respond t fallback; Lwt.return_unit))

let meta s = if s = "" then [] else [ X_protocol.Meta s ]

let finished t id =
  match List.partition (fun (i, _) -> i = id) t.running with
  | (_, o) :: _, rest -> t.running <- rest; if o <> None then t.last_owner <- o
  | [], _ -> ()

let post t : X_protocol.request -> unit = function
  | Eval (id, _line_number, code) ->
      (* Each phrase's output goes under it; stdout and stderr come once
         the cell is done. *)
      let stream = Jtw.eval_stream t.client code in
      let client = t.client in
      Lwt.async (fun () ->
          Lwt.finalize (fun () -> Lwt.catch
            (fun () ->
              Lwt_stream.iter
                (function
                  | Jtw.Phrase { loc; caml_ppf; _ } ->
                      if caml_ppf <> "" then
                        respond t (Top_response_at (id, loc, meta caml_ppf))
                  | Jtw.Done { stdout; stderr; caml_ppf; _ } ->
                      respond t
                        (Top_response
                           (id,
                            meta caml_ppf
                            @ (if stdout = "" then [] else [ X_protocol.Stdout stdout ])
                            @ if stderr = "" then [] else [ X_protocol.Stderr stderr ]))
                  | Jtw.Error msg -> respond t (Top_response (id, [ Stderr msg ])))
                stream)
            (fun exn ->
              respond t
                (Top_response (id, [ Stderr ("x-ocaml: " ^ Printexc.to_string exn) ]));
              Lwt.return_unit))
            (fun () -> if t.client == client then finished t id; Lwt.return_unit))
  | Merlin (id, Protocol.Complete_prefix (src, pos, filename)) ->
      answer t
        (fun () ->
          Lwt.map
            (fun c -> X_protocol.Merlin_response (id, Protocol.Completions (completions c)))
            (Jtw.complete ?filename t.client src (offset pos)))
        (Merlin_response (id, Protocol.Completions { from = 0; to_ = 0; entries = [] }))
  | Merlin (id, Protocol.Type_enclosing (src, pos, filename)) ->
      answer t
        (fun () ->
          Lwt.map
            (fun ts -> X_protocol.Merlin_response (id, Protocol.Typed_enclosings (List.map type_info ts)))
            (Jtw.type_at ?filename t.client src (offset pos)))
        (Merlin_response (id, Protocol.Typed_enclosings []))
  | Merlin (id, Protocol.All_errors (src, filename)) ->
      answer t
        (fun () ->
          Lwt.map
            (fun es -> X_protocol.Merlin_response (id, Protocol.Errors (List.map error es)))
            (Jtw.errors ?filename t.client src))
        (Merlin_response (id, Protocol.Errors []))
  | Merlin (id, Protocol.Add_cmis _) -> respond t (Merlin_response (id, Protocol.Added_cmis))
  (* js_top_worker does not format, and a cell needs no answer to keep its
     code as it is. *)
  | Format _ | Format_config _ | Setup -> ()

let eval ?owner ~id ~line_number t code =
  (* A cell run again makes its widgets again. *)
  Option.iter (Widget_render.clear_owned t.widgets) owner;
  t.running <- t.running @ [ (id, owner) ];
  post t (Eval (id, line_number, code))
let fmt ~id t code = post t (Format (id, code))
