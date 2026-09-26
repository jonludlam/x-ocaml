(* Each widget is a display of an <x-ocaml-view> element, with one callback
   through which the element sends the reader's actions. *)
open Js_of_ocaml

let json = Js.Unsafe.get Js.Unsafe.global (Js.string "JSON")
let parse s = Js.Unsafe.meth_call json "parse" [| Js.Unsafe.inject (Js.string s) |]
let stringify v = Js.to_string (Js.Unsafe.meth_call json "stringify" [| Js.Unsafe.inject v |])
let get o k = Js.Unsafe.get o (Js.string k)
let str o k = Js.to_string (get o k)
let quote s = stringify (Js.string s)

(* Where the page half is, relative to this worker. *)
let src = "x-ocaml-widgets.js"
let need tag = X_ocaml_lib.require ~tag ~src

type widget = {
  display : X_ocaml_lib.display;
  token : int;
  mutable view : string; (* the JSON of its view *)
  mutable commands : int;
  mutable command : string; (* the JSON of its last command, or "" *)
}

let widgets : (string, widget) Hashtbl.t = Hashtbl.create 8

let data id ~token ~view ~command =
  Printf.sprintf {|{"id":%s,"token":%d,"view":%s%s}|} (quote id) token view
    (if command = "" then "" else ",\"command\":" ^ command)

let refresh id w =
  X_ocaml_lib.update w.display (data id ~token:w.token ~view:w.view ~command:w.command)

let show id view =
  need "x-ocaml-view";
  let token =
    X_ocaml_lib.callback (fun payload ->
        let p = parse payload in
        let value = get p "value" in
        Widget.handle_event ~widget_id:id ~handler_id:(str p "handler")
          ~value:(if Js.Opt.test value then Some (Js.to_string value) else None))
  in
  (* When its cell runs again it goes, and the code drawing it can stop. *)
  let on_remove () =
    Hashtbl.remove widgets id;
    Widget.handle_event ~widget_id:id ~handler_id:"x-ocaml:removed" ~value:None
  in
  let display =
    X_ocaml_lib.display ~on_remove ~tag:"x-ocaml-view" (data id ~token ~view ~command:"")
  in
  Hashtbl.replace widgets id { display; token; view; commands = 0; command = "" }

let send msg =
  let o = parse msg in
  match str o "type" with
  | "widget_update" -> (
      let id = str o "widget_id" and view = stringify (get o "view") in
      match Hashtbl.find_opt widgets id with
      | Some w ->
          w.view <- view;
          refresh id w
      | None -> show id view)
  | "widget_clear" -> (
      let id = str o "widget_id" in
      match Hashtbl.find_opt widgets id with
      | Some w ->
          Hashtbl.remove widgets id;
          X_ocaml_lib.remove w.display
      | None -> ())
  | "widget_config" -> (
      (* A managed widget's new configuration is a new view. *)
      match Hashtbl.find_opt widgets (str o "widget_id") with
      | Some w ->
          let v = parse w.view in
          Js.Unsafe.set v (Js.string "config") (get o "config");
          w.view <- stringify v;
          refresh (str o "widget_id") w
      | None -> ())
  | "widget_command" -> (
      match Hashtbl.find_opt widgets (str o "widget_id") with
      | Some w ->
          w.commands <- w.commands + 1;
          w.command <-
            Printf.sprintf {|{"n":%d,"name":%s,"data":%s}|} w.commands
              (quote (str o "command")) (quote (str o "data"));
          refresh (str o "widget_id") w
      | None -> ())
  | "widget_register_adapter" ->
      need "x-ocaml-widget-adapter";
      ignore
        (X_ocaml_lib.display ~tag:"x-ocaml-widget-adapter"
           (Printf.sprintf {|{"kind":%s,"js":%s}|} (quote (str o "kind")) (quote (str o "js_code"))))
  | _ -> ()

let () = Widget.set_sender send
