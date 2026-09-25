open Brr

type send =
  widget_id:string ->
  handler_id:string ->
  event_type:string ->
  value:string option ->
  unit

(* -- views, decoded from the worker's JSON --------------------------------- *)

type attr =
  | Prop of string * string
  | Style of string * string
  | Class of string
  | Handler of string * string

type node =
  | Text of string
  | Element of { tag : string; attrs : attr list; children : node list }
  | Managed of { kind : string; config : string }

exception Malformed of string

let field o k =
  let v = Jv.get o k in
  if Jv.is_none v then raise (Malformed ("no field " ^ k)) else v

let str o k =
  let v = field o k in
  if Jstr.equal (Jv.typeof v) (Jstr.v "string") then Jv.to_string v
  else raise (Malformed (k ^ " is not a string"))

let list o k f =
  let v = Jv.get o k in
  if Jv.is_none v then []
  else if Jv.is_array v then List.map f (Jv.to_list Fun.id v)
  else raise (Malformed (k ^ " is not an array"))

let attr o =
  match str o "t" with
  | "prop" -> Prop (str o "k", str o "v")
  | "style" -> Style (str o "k", str o "v")
  | "cls" -> Class (str o "v")
  | "handler" -> Handler (str o "ev", str o "id")
  | t -> raise (Malformed ("unknown attribute " ^ t))

let rec node o =
  if Jv.is_none o then raise (Malformed "no node");
  match str o "t" with
  | "txt" -> Text (str o "v")
  | "el" ->
      Element
        {
          tag = str o "tag";
          attrs = list o "a" attr;
          children = list o "c" node;
        }
  | "managed" -> Managed { kind = str o "kind"; config = str o "config" }
  | t -> raise (Malformed ("unknown node " ^ t))

let decode v =
  try Ok (node v) with
  | Malformed m -> Error m
  | Jv.Error e -> Error (Jstr.to_string (Jv.Error.message e))

(* -- the DOM a view was rendered to -----------------------------------------

   Each update patches what is on the page rather than replacing it, so an
   element keeps its identity from one view to the next: an input keeps its
   focus, its selection and what the reader is typing. *)

type rendered =
  | R_text of { node : El.t; mutable text : string }
  | R_el of element

and element = {
  el : El.t;
  tag : string;
  mutable attrs : attr list;
  handlers : (string, string ref * Ev.listener) Hashtbl.t;
      (* event name -> the handler it currently fires, and the listener *)
  mutable sent : string list;
      (* values this input sent since its view last agreed with it *)
  mutable children : rendered list;
}

let dom = function R_text t -> t.node | R_el e -> e.el

let is_input tag =
  match String.lowercase_ascii tag with
  | "input" | "select" | "textarea" -> true
  | _ -> false

let is_checkbox el =
  match Jv.to_string (Jv.get (El.to_jv el) "type") with
  | "checkbox" | "radio" -> true
  | _ -> false

(* What an input says when it fires: a checkbox whether it is checked, as
   its value is fixed; anything else its value. *)
let current_value e =
  if not (is_input e.tag) then None
  else if is_checkbox e.el then
    Some (string_of_bool (Jv.to_bool (Jv.get (El.to_jv e.el) "checked")))
  else Some (Jv.to_string (Jv.get (El.to_jv e.el) "value"))

let listen ~widget_id ~(send : send) e ev handler_id =
  let current = ref handler_id in
  let listener =
    Ev.listen
      (Ev.Type.create (Jstr.v ev))
      (fun _ ->
        let value = current_value e in
        Option.iter
          (fun v -> e.sent <- v :: List.filteri (fun i _ -> i < 31) e.sent)
          value;
        send ~widget_id ~handler_id:!current ~event_type:ev ~value)
      (El.as_target e.el)
  in
  Hashtbl.replace e.handlers ev (current, listener)

let props attrs =
  List.filter_map (function Prop (k, v) -> Some (k, v) | _ -> None) attrs

let styles attrs =
  List.filter_map (function Style (k, v) -> Some (k, v) | _ -> None) attrs

let classes attrs =
  List.filter_map (function Class c -> Some c | _ -> None) attrs

let handlers attrs =
  List.filter_map (function Handler (e, h) -> Some (e, h) | _ -> None) attrs

(* What an input shows is its [value] and [checked] properties; once the
   reader has touched it, the attributes change nothing. Where a view gives
   them, it decides what the input shows, at every update, as a program that
   empties an input after Enter expects of a view that always says [""].

   Except for what the input itself sent. A program that echoes the input
   lags a keystroke or a click behind the reader, and writing its echo back
   would undo what they did since; so a value the input sent since its view
   last agreed with it is left alone. *)
let live e attrs =
  if not (is_input e.tag) then []
  else
    let checked =
      match List.assoc_opt "checked" attrs with
      | Some v -> Some (string_of_bool (v <> "false"))
      | None when List.mem_assoc "checked" (props e.attrs) -> Some "false"
      | None -> None
    in
    List.filter_map Fun.id
      [
        Option.map (fun v -> ("value", v)) (List.assoc_opt "value" attrs);
        Option.map (fun v -> ("checked", v)) checked;
      ]

let shown e = function
  | "checked" -> string_of_bool (Jv.to_bool (Jv.get (El.to_jv e.el) "checked"))
  | k -> Jv.to_string (Jv.get (El.to_jv e.el) k)

let set_live e (k, v) =
  if shown e k <> v && not (List.mem v e.sent) then
    Jv.set (El.to_jv e.el) k
      (if k = "checked" then Jv.of_bool (v = "true") else Jv.of_string v)

let apply_attrs ~widget_id ~send e attrs =
  let old = e.attrs in
  let op = props old and np = props attrs in
  let live = live e np in
  List.iter
    (fun (k, _) ->
      if not (List.mem_assoc k np) then begin
        El.set_at (Jstr.v k) None e.el
      end)
    op;
  List.iter
    (fun (k, v) ->
      if List.assoc_opt k op <> Some v then
        begin try El.set_at (Jstr.v k) (Some (Jstr.v v)) e.el
        with Jv.Error _ -> () (* not a valid attribute name *)
        end)
    np;
  let os = styles old and ns = styles attrs in
  List.iter
    (fun (k, _) ->
      if not (List.mem_assoc k ns) then
        El.set_inline_style (Jstr.v k) Jstr.empty e.el)
    os;
  List.iter (fun (k, v) -> El.set_inline_style (Jstr.v k) (Jstr.v v) e.el) ns;
  let oc = classes old and nc = classes attrs in
  let set_class c on =
    try El.set_class (Jstr.v c) on e.el with Jv.Error _ -> ()
  in
  List.iter (fun c -> if not (List.mem c nc) then set_class c false) oc;
  List.iter (fun c -> set_class c true) nc;
  let nh = handlers attrs in
  Hashtbl.filter_map_inplace
    (fun ev (current, listener) ->
      match List.assoc_opt ev nh with
      | Some h ->
          current := h;
          Some (current, listener)
      | None ->
          Ev.unlisten listener;
          None)
    e.handlers;
  List.iter
    (fun (ev, h) ->
      if not (Hashtbl.mem e.handlers ev) then listen ~widget_id ~send e ev h)
    nh;
  e.attrs <- attrs;
  List.iter (set_live e) live;
  (* Once the view and the input agree, what the input sent is behind us. *)
  if live <> [] && List.for_all (fun (k, v) -> shown e k = v) live then
    e.sent <- []

let placeholder text =
  El.span ~at:[ At.class' (Jstr.v "x-ocaml-widget-error") ] [ El.txt' text ]

let svg_ns = Jv.of_string "http://www.w3.org/2000/svg"

(* Whether children of [parent] are SVG: SVG's elements need their
   namespace, or they are inert HTML elements of the same name. *)
let svg_inside parent =
  let is_svg ns =
    (not (Jv.is_none ns)) && Jv.to_string ns = Jv.to_string svg_ns
  in
  (not (Jv.is_none parent))
  && is_svg (Jv.get parent "namespaceURI")
  && Jv.to_string (Jv.get parent "localName") <> "foreignObject"

let make_element ~svg tag =
  if svg || tag = "svg" then
    El.of_jv
      (Jv.call
         (Document.to_jv G.document)
         "createElementNS"
         [| svg_ns; Jv.of_string tag |])
  else El.v (Jstr.v tag) []

let rec render ?(svg = false) ~widget_id ~send = function
  | Text s -> R_text { node = El.txt' s; text = s }
  | Managed { kind; _ } ->
      (* Only a whole widget can be managed. *)
      broken ("a managed widget (" ^ kind ^ ") inside a view")
  | Element { tag; attrs; children } -> (
      match make_element ~svg tag with
      | exception Jv.Error _ -> broken ("no such element: " ^ tag)
      | el ->
          let e =
            {
              el;
              tag;
              attrs = [];
              handlers = Hashtbl.create 2;
              sent = [];
              children = [];
            }
          in
          apply_attrs ~widget_id ~send e attrs;
          let svg = svg_inside (El.to_jv el) in
          e.children <- List.map (render ~svg ~widget_id ~send) children;
          El.append_children el (List.map dom e.children);
          R_el e)

(* In place of what cannot be rendered; its empty tag matches no view, so
   the next update renders afresh. *)
and broken text =
  R_el
    {
      el = placeholder text;
      tag = "";
      attrs = [];
      handlers = Hashtbl.create 0;
      sent = [];
      children = [];
    }

let drop = function
  | R_el e -> Hashtbl.iter (fun _ (_, l) -> Ev.unlisten l) e.handlers
  | R_text _ -> ()

let rec patch ~widget_id ~send r n =
  match (r, n) with
  | R_text t, Text s ->
      if t.text <> s then begin
        Jv.set (El.to_jv t.node) "data" (Jv.of_string s);
        t.text <- s
      end;
      r
  | R_el e, Element { tag; attrs; children } when e.tag = tag ->
      apply_attrs ~widget_id ~send e attrs;
      e.children <- patch_children ~widget_id ~send e.el e.children children;
      r
  | _ ->
      let svg = svg_inside (Jv.get (El.to_jv (dom r)) "parentNode") in
      let fresh = render ~svg ~widget_id ~send n in
      rec_drop r;
      ignore
        (Jv.call (El.to_jv (dom r)) "replaceWith" [| El.to_jv (dom fresh) |]);
      fresh

and rec_drop r =
  drop r;
  match r with R_el e -> List.iter rec_drop e.children | R_text _ -> ()

and patch_children ~widget_id ~send parent old nodes =
  match (old, nodes) with
  | [], ns ->
      let svg = svg_inside (El.to_jv parent) in
      let fresh = List.map (render ~svg ~widget_id ~send) ns in
      El.append_children parent (List.map dom fresh);
      fresh
  | os, [] ->
      List.iter
        (fun r ->
          rec_drop r;
          El.remove (dom r))
        os;
      []
  | r :: os, n :: ns ->
      let r = patch ~widget_id ~send r n in
      r :: patch_children ~widget_id ~send parent os ns

(* -- widgets on the page --------------------------------------------------- *)

type adapter = {
  create : Jv.t -> string -> (string -> string option -> unit) -> Jv.t;
  update : Jv.t -> string -> unit;
  command : Jv.t -> string -> string -> unit;
  destroy : Jv.t -> unit;
}

type content =
  | Empty
  | View of rendered
  | Adapted of { kind : string; state : Jv.t }
  | Waiting of { kind : string; config : string }
(* a managed widget whose adapter has not arrived *)

type widget = {
  id : string;
  container : El.t;
  owner : El.t option;
  mutable content : content;
}

type t = {
  widgets : (string, widget) Hashtbl.t;
  adapters : (string, adapter) Hashtbl.t;
  mutable send : send;
}

let create () =
  {
    widgets = Hashtbl.create 8;
    adapters = Hashtbl.create 2;
    send = (fun ~widget_id:_ ~handler_id:_ ~event_type:_ ~value:_ -> ());
  }

let set_send t f = t.send <- f

let report what exn =
  Console.(error [ str ("x-ocaml: widget " ^ what ^ ":"); exn ])

let guard what f =
  try f () with
  | Jv.Error e -> report what (Jv.Error.message e |> Jv.of_jstr)
  | exn -> report what (Jv.of_string (Printexc.to_string exn))

let is_connected el = Jv.to_bool (Jv.get (El.to_jv el) "isConnected")

let destroy_content t w =
  match w.content with
  | View r -> rec_drop r
  | Adapted { kind; state } ->
      Option.iter
        (fun a -> guard ("destroy " ^ kind) (fun () -> a.destroy state))
        (Hashtbl.find_opt t.adapters kind)
  | Empty | Waiting _ -> ()

let remove t w =
  destroy_content t w;
  El.remove w.container;
  Hashtbl.remove t.widgets w.id

(* A cell's widgets follow it, in the order they were made. *)
let place container = function
  | Some cell when is_connected cell ->
      let rec last after =
        let next = Jv.get after "nextElementSibling" in
        if
          (not (Jv.is_none next))
          && Jv.to_bool
               (Jv.call (Jv.get next "classList") "contains"
                  [| Jv.of_string "x-ocaml-widget" |])
          && Jv.get next "xOcamlOwner" == El.to_jv cell
        then last next
        else after
      in
      ignore (Jv.call (last (El.to_jv cell)) "after" [| El.to_jv container |])
  | _ -> El.append_children (Document.body G.document) [ container ]

let widget t ~owner id =
  match Hashtbl.find_opt t.widgets id with
  | Some w when is_connected w.container -> w
  | stale ->
      Option.iter (remove t) stale;
      let container =
        El.div
          ~at:
            [
              At.class' (Jstr.v "x-ocaml-widget");
              At.v (Jstr.v "data-widget-id") (Jstr.v id);
            ]
          []
      in
      Option.iter
        (fun c -> Jv.set (El.to_jv container) "xOcamlOwner" (El.to_jv c))
        owner;
      place container owner;
      let w = { id; container; owner; content = Empty } in
      Hashtbl.replace t.widgets id w;
      w

let show_error w msg =
  w.content <- Empty;
  El.set_children w.container [ placeholder msg ]

let start_adapter t w kind config =
  match Hashtbl.find_opt t.adapters kind with
  | None ->
      w.content <- Waiting { kind; config };
      El.set_children w.container []
  | Some a -> (
      El.set_children w.container [];
      let send handler_id value =
        t.send ~widget_id:w.id ~handler_id ~event_type:handler_id ~value
      in
      try
        w.content <-
          Adapted { kind; state = a.create (El.to_jv w.container) config send }
      with exn ->
        let msg =
          match exn with
          | Jv.Error e -> Jstr.to_string (Jv.Error.message e)
          | e -> Printexc.to_string e
        in
        show_error w (Printf.sprintf "%s widget: %s" kind msg))

let update t ~owner id view =
  let w = widget t ~owner id in
  match decode view with
  | Error m ->
      destroy_content t w;
      show_error w ("malformed widget view: " ^ m)
  | Ok (Managed { kind; config }) -> (
      match w.content with
      | Adapted a when a.kind = kind ->
          guard ("update " ^ kind) (fun () ->
              (Hashtbl.find t.adapters kind).update a.state config)
      | _ ->
          destroy_content t w;
          start_adapter t w kind config)
  | Ok n -> (
      let send = t.send in
      match w.content with
      | View r -> w.content <- View (patch ~widget_id:id ~send r n)
      | _ ->
          destroy_content t w;
          let r = render ~widget_id:id ~send n in
          El.set_children w.container [ dom r ];
          w.content <- View r)

let config t id config =
  match Hashtbl.find_opt t.widgets id with
  | Some { content = Adapted { kind; state }; _ } ->
      guard ("update " ^ kind) (fun () ->
          (Hashtbl.find t.adapters kind).update state config)
  | Some ({ content = Waiting { kind; _ }; _ } as w) ->
      w.content <- Waiting { kind; config }
  | _ -> ()

let command t id cmd data =
  match Hashtbl.find_opt t.widgets id with
  | Some { content = Adapted { kind; state }; _ } ->
      guard ("command " ^ kind) (fun () ->
          (Hashtbl.find t.adapters kind).command state cmd data)
  | _ -> ()

let owner t id = Option.bind (Hashtbl.find_opt t.widgets id) (fun w -> w.owner)
let clear t id = Option.iter (remove t) (Hashtbl.find_opt t.widgets id)

let clear_all t =
  List.iter (remove t) (List.of_seq (Hashtbl.to_seq_values t.widgets))

let clear_owned t cell =
  List.iter
    (fun w ->
      match w.owner with Some c when c == cell -> remove t w | _ -> ())
    (List.of_seq (Hashtbl.to_seq_values t.widgets))

let register_adapter t kind js =
  match Jv.call Jv.global "eval" [| Jv.of_string js |] with
  | exception Jv.Error e ->
      report ("adapter " ^ kind) (Jv.of_jstr (Jv.Error.message e))
  | obj when Jv.is_none obj ->
      report ("adapter " ^ kind) (Jv.of_string "the code returned nothing")
  | obj ->
      let meth name args =
        let f = Jv.get obj name in
        if Jv.is_none f then Jv.undefined else Jv.call obj name args
      in
      let a =
        {
          create =
            (fun container config send ->
              let js_send =
                Jv.callback ~arity:2 (fun handler_id value ->
                    let str v =
                      Jv.to_string (Jv.call Jv.global "String" [| v |])
                    in
                    send (str handler_id)
                      (if Jv.is_none value then None else Some (str value)))
              in
              Jv.call obj "create" [| container; Jv.of_string config; js_send |]);
          update =
            (fun state config ->
              ignore (meth "update" [| state; Jv.of_string config |]));
          command =
            (fun state cmd data ->
              ignore
                (meth "command"
                   [| state; Jv.of_string cmd; Jv.of_string data |]));
          destroy = (fun state -> ignore (meth "destroy" [| state |]));
        }
      in
      Hashtbl.replace t.adapters kind a;
      (* Widgets shown before their adapter came can start now. *)
      Hashtbl.iter
        (fun _ w ->
          match w.content with
          | Waiting { kind = k; config } when k = kind ->
              start_adapter t w kind config
          | _ -> ())
        t.widgets
