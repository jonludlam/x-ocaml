(* Each widget is a display of an <x-ocaml-view> element, with one callback
   through which the element sends the reader's actions. *)
open Js_of_ocaml

module View = struct
  type event_id = string

  type attr =
    | Property of string * string
    | Style of string * string
    | Class of string
    | Handler of string * event_id

  type node =
    | Text of string
    | Element of { tag : string; attrs : attr list; children : node list }
    | Managed of { kind : string; config : string }
end

(* JSON, through the browser's own. *)

let json = Js.Unsafe.get Js.Unsafe.global (Js.string "JSON")

let parse s =
  Js.Unsafe.meth_call json "parse" [| Js.Unsafe.inject (Js.string s) |]

let stringify v =
  Js.to_string (Js.Unsafe.meth_call json "stringify" [| Js.Unsafe.inject v |])

let get o k = Js.Unsafe.get o (Js.string k)
let str o k = Js.to_string (get o k)
let quote s = stringify (Js.string s)

let obj fields =
  Js.Unsafe.obj
    (Array.of_list (List.map (fun (k, v) -> (k, Js.Unsafe.inject v)) fields))

let jstr s = Js.Unsafe.inject (Js.string s)

let array f l =
  Js.Unsafe.inject
    (Js.array (Array.of_list (List.map (fun x -> Js.Unsafe.inject (f x)) l)))

(* Views as the page half reads them. *)

let json_of_attr : View.attr -> _ = function
  | Property (k, v) -> obj [ ("t", jstr "prop"); ("k", jstr k); ("v", jstr v) ]
  | Style (k, v) -> obj [ ("t", jstr "style"); ("k", jstr k); ("v", jstr v) ]
  | Class c -> obj [ ("t", jstr "cls"); ("v", jstr c) ]
  | Handler (ev, id) ->
      obj [ ("t", jstr "handler"); ("ev", jstr ev); ("id", jstr id) ]

let rec json_of_node : View.node -> _ = function
  | Text s -> obj [ ("t", jstr "txt"); ("v", jstr s) ]
  | Element { tag; attrs; children } ->
      obj
        [
          ("t", jstr "el");
          ("tag", jstr tag);
          ("a", array json_of_attr attrs);
          ("c", array json_of_node children);
        ]
  | Managed { kind; config } ->
      obj [ ("t", jstr "managed"); ("kind", jstr kind); ("config", jstr config) ]

let view_json view = stringify (json_of_node view)

(* Widgets *)

(* Where the page half is, relative to the worker. *)
let src = "x-ocaml-widgets.js"
let need tag = X_ocaml_lib.require ~tag ~src

type widget = {
  display : X_ocaml_lib.display;
  token : X_ocaml_lib.token;
  mutable handlers : (string * (string option -> unit)) list;
  mutable view : string;  (** The JSON of its view. *)
  mutable commands : int;
  mutable command : string;  (** The JSON of its last command, or "". *)
}

let widgets : (string, widget) Hashtbl.t = Hashtbl.create 8

let data id ~token ~view ~command =
  Printf.sprintf {|{"id":%s,"token":%s,"view":%s%s}|} (quote id)
    (X_ocaml_lib.string_of_token token)
    view
    (if command = "" then "" else ",\"command\":" ^ command)

let refresh id w =
  X_ocaml_lib.update w.display
    (data id ~token:w.token ~view:w.view ~command:w.command)

let call handlers handler value =
  Option.iter (fun f -> f value) (List.assoc_opt handler handlers)

let show id ~handlers view =
  need "x-ocaml-view";
  let w = ref None in
  let handlers_now () =
    match !w with Some w -> w.handlers | None -> handlers
  in
  let token =
    X_ocaml_lib.callback (fun payload ->
        let p = parse payload in
        let value = get p "value" in
        call (handlers_now ()) (str p "handler")
          (if Js.Opt.test value then Some (Js.to_string value) else None))
  in
  let on_remove () =
    let handlers = handlers_now () in
    Hashtbl.remove widgets id;
    call handlers "x-ocaml:removed" None
  in
  let display =
    X_ocaml_lib.display ~on_remove ~tag:"x-ocaml-view"
      (data id ~token ~view ~command:"")
  in
  let widget = { display; token; handlers; view; commands = 0; command = "" } in
  w := Some widget;
  Hashtbl.replace widgets id widget

let display ~id ~handlers view =
  let view = view_json view in
  match Hashtbl.find_opt widgets id with
  | Some w ->
      w.handlers <- handlers;
      w.view <- view;
      refresh id w
  | None -> show id ~handlers view

let update ~id view =
  let view = view_json view in
  match Hashtbl.find_opt widgets id with
  | Some w ->
      w.view <- view;
      refresh id w
  | None -> show id ~handlers:[] view

let clear ~id =
  match Hashtbl.find_opt widgets id with
  | Some w ->
      Hashtbl.remove widgets id;
      X_ocaml_lib.remove w.display
  | None -> ()

let display_managed ~id ~kind ~config ~handlers =
  display ~id ~handlers (Managed { kind; config })

(* A managed widget's new configuration is a new view. *)
let update_config ~id config =
  match Hashtbl.find_opt widgets id with
  | Some w ->
      let v = parse w.view in
      Js.Unsafe.set v (Js.string "config") (Js.string config);
      w.view <- stringify v;
      refresh id w
  | None -> ()

let command ~id name data =
  match Hashtbl.find_opt widgets id with
  | Some w ->
      w.commands <- w.commands + 1;
      w.command <-
        Printf.sprintf {|{"n":%d,"name":%s,"data":%s}|} w.commands (quote name)
          (quote data);
      refresh id w
  | None -> ()

let register_adapter ~kind ~js =
  need "x-ocaml-widget-adapter";
  ignore
    (X_ocaml_lib.display ~tag:"x-ocaml-widget-adapter"
       (Printf.sprintf {|{"kind":%s,"js":%s}|} (quote kind) (quote js)))
