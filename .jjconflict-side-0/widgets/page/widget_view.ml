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

type adapter = {
  create : Jv.t -> string -> (string -> string option -> unit) -> Jv.t;
  update : Jv.t -> string -> unit;
  command : Jv.t -> string -> string -> unit;
  destroy : Jv.t -> unit;
}

type rendered =
  | R_text of { node : El.t; mutable text : string }
  | R_el of element
  | R_managed of managed

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

(* A managed widget inside a view: its adapter draws into [mel]. *)
and managed = {
  mel : El.t;
  kind : string;
  mutable config : string;
  mutable state : Jv.t option;  (** [None] until its adapter comes *)
}

(* What rendering needs: whose events these are, where they go, and the
   adapters for managed widgets. *)
type ctx = {
  widget_id : string;
  send : send;
  adapters : (string, adapter) Hashtbl.t;
}

let dom = function R_text t -> t.node | R_el e -> e.el | R_managed m -> m.mel

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
  if is_checkbox e.el then
    string_of_bool (Jv.to_bool (Jv.get (El.to_jv e.el) "checked"))
  else Jv.to_string (Jv.get (El.to_jv e.el) "value")

let report what exn =
  Console.(error [ str ("x-ocaml: widget " ^ what ^ ":"); exn ])

let message = function
  | Jv.Error e -> Jstr.to_string (Jv.Error.message e)
  | e -> Printexc.to_string e

(* -- what an event says -----------------------------------------------------

   An input's events say what is in it. Keyboard events say which key, and
   mouse, pointer, touch and wheel events where: as JSON, with the position
   relative to the element, or for SVG in the user units of its [<svg>], so
   that a chart can map a click back to its data. *)

let is_key ev = String.length ev > 3 && String.sub ev 0 3 = "key"
let flag jv k = Jv.of_bool (Jv.to_bool (Jv.get jv k))

let num jv k =
  match Jv.get jv k with v when Jv.is_none v -> 0. | v -> Jv.to_float v

let modifiers ev =
  [
    ("shift", flag ev "shiftKey");
    ("alt", flag ev "altKey");
    ("ctrl", flag ev "ctrlKey");
    ("meta", flag ev "metaKey");
  ]

let key_details e ev =
  let value =
    if is_input e.tag then [ ("value", Jv.of_string (current_value e)) ] else []
  in
  Jv.obj
    (Array.of_list
       ([
          ("key", Jv.get ev "key");
          ("code", Jv.get ev "code");
          ("repeat", flag ev "repeat");
        ]
       @ modifiers ev @ value))

let position e ev =
  (* A touch event's position is its first touch's. *)
  let at =
    match Jv.get ev "touches" with
    | t when (not (Jv.is_none t)) && Jv.to_int (Jv.get t "length") > 0 ->
        Jv.call t "item" [| Jv.of_int 0 |]
    | t when not (Jv.is_none t) ->
        Jv.call (Jv.get ev "changedTouches") "item" [| Jv.of_int 0 |]
    | _ -> ev
  in
  let cx = num at "clientX" and cy = num at "clientY" in
  let el = El.to_jv e.el in
  let svg =
    if Jv.to_string (Jv.get el "localName") = "svg" then el
    else Jv.get el "ownerSVGElement"
  in
  let box = Jv.call el "getBoundingClientRect" [||] in
  let in_box () =
    ( cx -. num box "left",
      cy -. num box "top",
      num box "width",
      num box "height" )
  in
  if Jv.is_none svg then in_box ()
  else
    match Jv.call svg "getScreenCTM" [||] with
    | ctm when Jv.is_none ctm -> in_box ()
    | ctm ->
        let p =
          Jv.new'
            (Jv.get Jv.global "DOMPoint")
            [| Jv.of_float cx; Jv.of_float cy |]
        in
        let p = Jv.call p "matrixTransform" [| Jv.call ctm "inverse" [||] |] in
        let vb = Jv.get (Jv.get svg "viewBox") "baseVal" in
        let w, h =
          if (not (Jv.is_none vb)) && num vb "width" > 0. then
            (num vb "width", num vb "height")
          else (num box "width", num box "height")
        in
        (num p "x", num p "y", w, h)

let pointer_details e ev =
  let x, y, w, h = position e ev in
  let wheel =
    if Jv.is_none (Jv.get ev "deltaY") then []
    else [ ("dx", Jv.get ev "deltaX"); ("dy", Jv.get ev "deltaY") ]
  in
  Jv.obj
    (Array.of_list
       ([
          ("x", Jv.of_float x);
          ("y", Jv.of_float y);
          ("width", Jv.of_float w);
          ("height", Jv.of_float h);
          ("button", Jv.of_float (num ev "button"));
          ("buttons", Jv.of_float (num ev "buttons"));
        ]
       @ modifiers ev @ wheel))

let details e ev_name ev =
  let json v = Some (Jstr.to_string (Json.encode v)) in
  if is_key ev_name then json (key_details e ev)
  else if is_input e.tag then Some (current_value e)
  else if
    (not (Jv.is_none (Jv.get ev "clientX")))
    || not (Jv.is_none (Jv.get ev "touches"))
  then json (pointer_details e ev)
  else None

(* Events that come in floods go at most once a frame, with the latest. *)
let coalesced = function
  | "pointermove" | "mousemove" | "touchmove" | "wheel" | "drag" | "scroll" ->
      true
  | _ -> false

(* [data-x-ocaml-prevent]: the events, among those it handles, whose default
   action an element prevents: a game's arrow keys, a chart's wheel. *)
let prevents e ev =
  match
    List.assoc_opt "data-x-ocaml-prevent"
      (List.filter_map
         (function Prop (k, v) -> Some (k, v) | _ -> None)
         e.attrs)
  with
  | Some evs -> List.mem ev (String.split_on_char ' ' evs)
  | None -> false

let listen ctx e ev handler_id =
  let current = ref handler_id in
  let pending = ref None in
  let fire value =
    ctx.send ~widget_id:ctx.widget_id ~handler_id:!current ~event_type:ev ~value
  in
  let listener =
    Ev.listen
      (Ev.Type.create (Jstr.v ev))
      (fun event ->
        let jv = Ev.to_jv event in
        if prevents e ev then Jv.call jv "preventDefault" [||] |> ignore;
        let value =
          try details e ev jv
          with exn ->
            report "event" (Jv.of_string (message exn));
            None
        in
        if is_input e.tag && not (is_key ev) then
          Option.iter
            (fun v -> e.sent <- v :: List.filteri (fun i _ -> i < 31) e.sent)
            value;
        if not (coalesced ev) then fire value
        else begin
          if !pending = None then
            ignore
              (G.request_animation_frame (fun _ ->
                   let v = Option.join !pending in
                   pending := None;
                   fire v));
          pending := Some value
        end)
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

let apply_attrs ctx e attrs =
  let old = e.attrs in
  let op = props old and np = props attrs in
  let live = live e np in
  List.iter
    (fun (k, _) ->
      if not (List.mem_assoc k np) then El.set_at (Jstr.v k) None e.el)
    op;
  List.iter
    (fun (k, v) ->
      if List.assoc_opt k op <> Some v then
        try El.set_at (Jstr.v k) (Some (Jstr.v v)) e.el
        with Jv.Error _ -> () (* not a valid attribute name *))
    np;
  let os = styles old and ns = styles attrs in
  List.iter
    (fun (k, _) ->
      if not (List.mem_assoc k ns) then
        El.set_inline_style (Jstr.v k) Jstr.empty e.el)
    os;
  List.iter
    (fun (k, v) ->
      if List.assoc_opt k os <> Some v then
        El.set_inline_style (Jstr.v k) (Jstr.v v) e.el)
    ns;
  let oc = classes old and nc = classes attrs in
  let set_class c on =
    try El.set_class (Jstr.v c) on e.el with Jv.Error _ -> ()
  in
  List.iter (fun c -> if not (List.mem c nc) then set_class c false) oc;
  List.iter (fun c -> if not (List.mem c oc) then set_class c true) nc;
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
    (fun (ev, h) -> if not (Hashtbl.mem e.handlers ev) then listen ctx e ev h)
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

(* Start a managed widget's adapter, if it has come. Its events go to the
   handlers of the widget it is in. *)
let start ctx m =
  match Hashtbl.find_opt ctx.adapters m.kind with
  | None -> ()
  | Some a -> (
      let send handler_id value =
        ctx.send ~widget_id:ctx.widget_id ~handler_id ~event_type:handler_id
          ~value
      in
      El.set_children m.mel [];
      try m.state <- Some (a.create (El.to_jv m.mel) m.config send)
      with exn ->
        El.set_children m.mel
          [ placeholder (Printf.sprintf "%s widget: %s" m.kind (message exn)) ])

let rec render ?(svg = false) ctx = function
  | Text s -> R_text { node = El.txt' s; text = s }
  | Managed { kind; config } ->
      let mel = El.div ~at:[ At.class' (Jstr.v "x-ocaml-managed") ] [] in
      let m = { mel; kind; config; state = None } in
      start ctx m;
      R_managed m
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
          apply_attrs ctx e attrs;
          let svg = svg_inside (El.to_jv el) in
          e.children <- List.map (render ~svg ctx) children;
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

let rec drop ctx = function
  | R_el e ->
      Hashtbl.iter (fun _ (_, l) -> Ev.unlisten l) e.handlers;
      List.iter (drop ctx) e.children
  | R_text _ -> ()
  | R_managed { kind; state = Some state; _ } -> (
      match Hashtbl.find_opt ctx.adapters kind with
      | Some a -> (
          try a.destroy state
          with exn -> report ("destroy " ^ kind) (Jv.of_string (message exn)))
      | None -> ())
  | R_managed _ -> ()

let rec patch ctx r n =
  match (r, n) with
  | R_text t, Text s ->
      if t.text <> s then begin
        Jv.set (El.to_jv t.node) "data" (Jv.of_string s);
        t.text <- s
      end;
      r
  | R_el e, Element { tag; attrs; children } when e.tag = tag ->
      apply_attrs ctx e attrs;
      e.children <- patch_children ctx e.el e.children children;
      r
  | R_managed m, Managed { kind; config } when m.kind = kind ->
      if m.config <> config then begin
        m.config <- config;
        match (m.state, Hashtbl.find_opt ctx.adapters kind) with
        | Some state, Some a -> (
            try a.update state config
            with exn -> report ("update " ^ kind) (Jv.of_string (message exn)))
        | _ -> ()
      end;
      r
  | _ ->
      let svg = svg_inside (Jv.get (El.to_jv (dom r)) "parentNode") in
      let fresh = render ~svg ctx n in
      drop ctx r;
      ignore
        (Jv.call (El.to_jv (dom r)) "replaceWith" [| El.to_jv (dom fresh) |]);
      fresh

and patch_children ctx parent old nodes =
  match (old, nodes) with
  | [], ns ->
      let svg = svg_inside (El.to_jv parent) in
      let fresh = List.map (render ~svg ctx) ns in
      El.append_children parent (List.map dom fresh);
      fresh
  | os, [] ->
      List.iter
        (fun r ->
          drop ctx r;
          El.remove (dom r))
        os;
      []
  | r :: os, n :: ns ->
      let r = patch ctx r n in
      r :: patch_children ctx parent os ns

(* Managed widgets waiting for the adapter of [kind]. *)
let rec waiting kind = function
  | R_managed ({ state = None; _ } as m) when m.kind = kind -> [ m ]
  | R_el e -> List.concat_map (waiting kind) e.children
  | _ -> []

(* -- adapters ----------------------------------------------------------------

   Managed widgets are drawn by adapters: JavaScript that code in the worker
   sends, which evaluates to an object with create, update, command and
   destroy methods. *)

let adapters : (string, adapter) Hashtbl.t = Hashtbl.create 4

let register_adapter kind js =
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
      Hashtbl.replace adapters kind
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

let command ctx r cmd data =
  match r with
  | R_managed { kind; state = Some state; _ } -> (
      match Hashtbl.find_opt ctx.adapters kind with
      | Some a -> (
          try a.command state cmd data
          with exn -> report ("command " ^ kind) (Jv.of_string (message exn)))
      | None -> ())
  | _ -> ()
