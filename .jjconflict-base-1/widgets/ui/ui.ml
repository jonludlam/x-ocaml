(* A widget toolkit on js_top_worker's Widget library. A widget is a
   function from a render context to a view; a root re-renders, and sends its
   view again, whenever a variable it read while rendering changes. *)

module View = Widget.View

(* Equality that works on anything: [=] raises on functions, and a value
   holding one (a choice of functions, a widget) is only ever itself. *)
let same a b = a == b || try a = b with Invalid_argument _ -> false

let fresh =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

(* -- variables ------------------------------------------------------------ *)

module Var = struct
  type 'a t = {
    mutable value : 'a;
    eq : 'a -> 'a -> bool;
    mutable watchers : ('a -> unit) list;
    readers : (int, unit -> unit) Hashtbl.t;
        (* the roots that read it when they last rendered *)
  }

  (* The root rendering now, which reads subscribe. *)
  let current : (int * (unit -> unit)) option ref = ref None
  let create ?(eq = same) value = { value; eq; watchers = []; readers = Hashtbl.create 2 }

  let get v =
    Option.iter (fun (id, inv) -> Hashtbl.replace v.readers id inv) !current;
    v.value

  let peek v = v.value

  let set v x =
    if not (v.eq v.value x) then begin
      v.value <- x;
      List.iter (fun f -> f x) v.watchers;
      let readers = List.of_seq (Hashtbl.to_seq_values v.readers) in
      Hashtbl.reset v.readers;
      List.iter (fun inv -> inv ()) readers
    end

  let update v f = set v (f v.value)
  let watch v f = v.watchers <- v.watchers @ [ f ]

  let map ?eq f v =
    let w = create ?eq (f v.value) in
    watch v (fun x -> set w (f x));
    w

  let map2 ?eq f a b =
    let w = create ?eq (f a.value b.value) in
    watch a (fun x -> set w (f x b.value));
    watch b (fun y -> set w (f a.value y));
    w

  (* Changes made together render once, at the end. *)
  let depth = ref 0
  let dirty : (int, unit -> unit) Hashtbl.t = Hashtbl.create 8

  let batch f =
    incr depth;
    Fun.protect
      ~finally:(fun () ->
        decr depth;
        if !depth = 0 then
          (* Rendering can change variables, and so dirty other roots. *)
          let rec flush n =
            if Hashtbl.length dirty > 0 && n < 100 then begin
              let roots = List.of_seq (Hashtbl.to_seq_values dirty) in
              Hashtbl.reset dirty;
              List.iter (fun r -> r ()) roots;
              flush (n + 1)
            end
          in
          flush 0)
      f
end

let link a b =
  Var.set b (Var.peek a);
  Var.watch a (Var.set b);
  Var.watch b (Var.set a)

let dlink ~f a b =
  Var.set b (f (Var.peek a));
  Var.watch a (fun x -> Var.set b (f x))

(* -- events --------------------------------------------------------------- *)

(* The client sends small flat JSON objects; this reads them. *)
module Json = struct
  type v = S of string | N of float | B of bool | Null

  exception Bad

  let parse s =
    let n = String.length s and i = ref 0 in
    let peek () = if !i < n then s.[!i] else raise Bad in
    let skip () = while !i < n && (s.[!i] = ' ' || s.[!i] = '\n' || s.[!i] = '\t' || s.[!i] = '\r') do incr i done in
    let expect c = skip (); if peek () <> c then raise Bad; incr i in
    let string () =
      expect '"';
      let b = Buffer.create 16 in
      let rec go () =
        match peek () with
        | '"' -> incr i
        | '\\' ->
            incr i;
            (match peek () with
             | 'n' -> Buffer.add_char b '\n'
             | 't' -> Buffer.add_char b '\t'
             | 'r' -> Buffer.add_char b '\r'
             | 'b' -> Buffer.add_char b '\b'
             | 'f' -> Buffer.add_char b '\012'
             | 'u' ->
                 let code = int_of_string ("0x" ^ String.sub s (!i + 1) 4) in
                 i := !i + 4;
                 Buffer.add_utf_8_uchar b (try Uchar.of_int code with _ -> Uchar.rep)
             | c -> Buffer.add_char b c);
            incr i;
            go ()
        | c ->
            Buffer.add_char b c;
            incr i;
            go ()
      in
      go ();
      Buffer.contents b
    in
    let value () =
      skip ();
      match peek () with
      | '"' -> S (string ())
      | 't' -> i := !i + 4; B true
      | 'f' -> i := !i + 5; B false
      | 'n' -> i := !i + 4; Null
      | _ ->
          let start = !i in
          while !i < n && String.contains "+-0123456789.eE" s.[!i] do incr i done;
          (match float_of_string_opt (String.sub s start (!i - start)) with
           | Some f -> N f
           | None -> raise Bad)
    in
    try
      expect '{';
      skip ();
      if peek () = '}' then []
      else
        let rec fields acc =
          let k = string () in
          expect ':';
          let v = value () in
          skip ();
          match peek () with
          | ',' -> incr i; skip (); fields ((k, v) :: acc)
          | '}' -> List.rev ((k, v) :: acc)
          | _ -> raise Bad
        in
        skip ();
        fields []
    with Bad | Invalid_argument _ | Failure _ -> []

  let str s = "\"" ^ String.concat "" (List.map (function
      | '"' -> "\\\"" | '\\' -> "\\\\" | '\n' -> "\\n"
      | c when Char.code c < 0x20 -> Printf.sprintf "\\u%04x" (Char.code c)
      | c -> String.make 1 c) (List.init (String.length s) (String.get s))) ^ "\""
end

module Event = struct
  type t = {
    value : string option;
    x : float;
    y : float;
    width : float;
    height : float;
    key : string;
    shift : bool;
    alt : bool;
    ctrl : bool;
    meta : bool;
    buttons : int;
    dx : float;
    dy : float;
  }

  let of_value value =
    let fields = match value with Some s when String.length s > 0 && s.[0] = '{' -> Json.parse s | _ -> [] in
    let num k = match List.assoc_opt k fields with Some (Json.N f) -> f | _ -> 0. in
    let bool k = match List.assoc_opt k fields with Some (Json.B b) -> b | _ -> false in
    let str k = match List.assoc_opt k fields with Some (Json.S s) -> Some s | _ -> None in
    {
      value = (if fields = [] then value else str "value");
      x = num "x";
      y = num "y";
      width = num "width";
      height = num "height";
      key = Option.value (str "key") ~default:"";
      shift = bool "shift";
      alt = bool "alt";
      ctrl = bool "ctrl";
      meta = bool "meta";
      buttons = int_of_float (num "buttons");
      dx = num "dx";
      dy = num "dy";
    }
end

(* -- widgets -------------------------------------------------------------- *)

type ctx = { root : string; handlers : (string, string option -> unit) Hashtbl.t }
type t = ctx -> View.node

(* A handler, under a key its widget makes unique; the view names it. *)
let on ctx key f =
  Hashtbl.replace ctx.handlers key f;
  key

let key uid name = Printf.sprintf "%d.%s" uid name

let el ?(cls = []) ?(style = []) ?(attrs = []) ?(handlers = []) tag children : View.node =
  Element
    {
      tag;
      attrs =
        List.map (fun c -> View.Class c) cls
        @ List.map (fun (k, v) -> View.Style (k, v)) style
        @ List.map (fun (k, v) -> View.Property (k, v)) attrs
        @ List.map (fun (e, h) -> View.Handler (e, h)) handlers;
      children;
    }

let txt s : View.node = Text s
let node n : t = fun _ -> n

let on_event ctx uid ev f = on ctx (key uid ev) (fun v -> f (Event.of_value v))

let element ?cls ?style ?attrs ?(on = []) tag children : t =
  let uid = fresh () in
  fun ctx ->
    let handlers = List.map (fun (ev, f) -> (ev, on_event ctx uid ev f)) on in
    el ?cls ?style ?attrs ~handlers tag (List.map (fun c -> c ctx) children)

let text s : t = fun _ -> txt s
let pre s : t = fun _ -> el ~cls:[ "xui-pre" ] "pre" [ txt s ]
let title s : t = fun _ -> el ~cls:[ "xui-title" ] "div" [ txt s ]
let note s : t = fun _ -> el ~cls:[ "xui-note" ] "div" [ txt s ]
let dyn f : t = fun ctx -> (f ()) ctx
let text_of f v : t = fun _ -> txt (f (Var.get v))
let badge ?(kind = "info") s : t = fun _ -> el ~cls:[ "xui-badge"; "xui-" ^ kind ] "span" [ txt s ]

(* A row: its label, the control, and what it is set to. *)
let row ?label ?readout control =
  el ~cls:[ "xui-row" ] "div"
    ((match label with Some l -> [ el ~cls:[ "xui-label" ] "label" [ txt l ] ] | None -> [])
    @ [ el ~cls:[ "xui-control" ] "div" [ control ] ]
    @ match readout with Some r -> [ el ~cls:[ "xui-readout" ] "span" [ txt r ] ] | None -> [])

let dis disabled = if disabled then [ ("disabled", "") ] else []

let fmt_float ?(decimals = 2) f =
  let s = Printf.sprintf "%.*f" decimals f in
  if String.contains s '.' then
    let n = ref (String.length s) in
    while !n > 0 && s.[!n - 1] = '0' do decr n done;
    if !n > 0 && s.[!n - 1] = '.' then decr n;
    String.sub s 0 !n
  else s

(* -- numbers -------------------------------------------------------------- *)

(* A number as an input's attribute: HTML's grammar has no "3." (the input
   would take its default), so the shortest form that reads back exactly. *)
let num x =
  let s = Printf.sprintf "%.15g" x in
  if float_of_string s = x then s else Printf.sprintf "%.17g" x

(* How far along its track a slider is, for the filled part of the track. *)
let fill lo hi x =
  let f = if hi > lo then (x -. lo) /. (hi -. lo) else 0. in
  ("--xui-fill", Printf.sprintf "%.2f%%" (100. *. Float.min 1. (Float.max 0. f)))

let int_slider ?label ?(min = 0) ?(max = 100) ?(step = 1) ?(disabled = false) v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s -> Option.iter (fun n -> Var.set v n) (Option.bind s int_of_string_opt)) in
    let x = Var.get v in
    row ?label ~readout:(string_of_int x)
      (el "input" ~cls:[ "xui-slider" ] ~style:[ fill (float min) (float max) (float x) ]
         ~attrs:([ ("type", "range"); ("min", string_of_int min); ("max", string_of_int max);
                   ("step", string_of_int step); ("value", string_of_int x) ] @ dis disabled)
         ~handlers:[ ("input", h) ] [])

let float_slider ?label ?(min = 0.) ?(max = 1.) ?(step = 0.01) ?decimals ?(disabled = false) v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s -> Option.iter (Var.set v) (Option.bind s float_of_string_opt)) in
    let x = Var.get v in
    let decimals = match decimals with Some d -> d | None -> Stdlib.max 0 (int_of_float (ceil (-. log10 step))) in
    row ?label ~readout:(fmt_float ~decimals x)
      (el "input" ~cls:[ "xui-slider" ] ~style:[ fill min max x ]
         ~attrs:([ ("type", "range"); ("min", num min); ("max", num max);
                   ("step", num step); ("value", num x) ] @ dis disabled)
         ~handlers:[ ("input", h) ] [])

let log_slider ?label ?(base = 10.) ?(min = -3.) ?(max = 3.) ?(step = 0.01) v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s ->
        Option.iter (fun e -> Var.set v (base ** e)) (Option.bind s float_of_string_opt)) in
    let x = Var.get v in
    let e = log x /. log base in
    row ?label ~readout:(Printf.sprintf "%.3g" x)
      (el "input" ~cls:[ "xui-slider" ] ~style:[ fill min max e ]
         ~attrs:[ ("type", "range"); ("min", num min); ("max", num max);
                  ("step", num step); ("value", num e) ]
         ~handlers:[ ("input", h) ] [])

let range_slider ?label ?(min = 0) ?(max = 100) ?(step = 1) v : t =
  let uid = fresh () in
  fun ctx ->
    let lo, hi = Var.get v in
    let set_lo s = Option.iter (fun n -> let _, hi = Var.peek v in Var.set v (Stdlib.min n hi, hi)) (Option.bind s int_of_string_opt) in
    let set_hi s = Option.iter (fun n -> let lo, _ = Var.peek v in Var.set v (lo, Stdlib.max n lo)) (Option.bind s int_of_string_opt) in
    let thumb name value h =
      el "input" ~cls:[ "xui-slider"; "xui-thumb-only" ]
        ~attrs:[ ("type", "range"); ("min", string_of_int min); ("max", string_of_int max);
                 ("step", string_of_int step); ("value", string_of_int value); ("data-thumb", name) ]
        ~handlers:[ ("input", h) ] []
    in
    let pct x = Printf.sprintf "%.3f%%" (100. *. float (x - min) /. float (Stdlib.max 1 (max - min))) in
    row ?label ~readout:(Printf.sprintf "%d – %d" lo hi)
      (el ~cls:[ "xui-range" ] "div"
         [
           el ~cls:[ "xui-range-track" ] "div" [];
           el ~cls:[ "xui-range-fill" ] ~style:[ ("left", pct lo); ("width", Printf.sprintf "calc(%s - %s)" (pct hi) (pct lo)) ] "div" [];
           thumb "lo" lo (on ctx (key uid "lo") set_lo);
           thumb "hi" hi (on ctx (key uid "hi") set_hi);
         ])

let number_input ~parse ~show ?label ?min ?max ?step v : t =
  let uid = fresh () in
  fun ctx ->
    let clamp x =
      let x = match min with Some m when x < m -> m | _ -> x in
      match max with Some m when x > m -> m | _ -> x
    in
    let h = on ctx (key uid "change") (fun s -> Option.iter (fun x -> Var.set v (clamp x)) (Option.bind s parse)) in
    let opt k = function Some x -> [ (k, show x) ] | None -> [] in
    row ?label
      (el "input" ~cls:[ "xui-input"; "xui-number" ]
         ~attrs:([ ("type", "number"); ("value", show (Var.get v)) ] @ opt "min" min @ opt "max" max @ opt "step" step)
         ~handlers:[ ("change", h) ] [])

let int_input ?label ?min ?max ?step v =
  number_input ~parse:int_of_string_opt ~show:string_of_int ?label ?min ?max ?step v

let float_input ?label ?min ?max ?step v =
  number_input ~parse:float_of_string_opt ~show:(fun f -> fmt_float ~decimals:6 f) ?label ?min ?max ?step v

let progress ?label ?(max = 1.) ?(kind = "info") v : t =
 fun _ ->
  let x = Var.get v in
  let frac = Float.min 1. (Float.max 0. (x /. max)) in
  row ?label ~readout:(Printf.sprintf "%.0f%%" (100. *. frac))
    (el ~cls:[ "xui-progress" ] "div"
       [ el ~cls:[ "xui-progress-bar"; "xui-" ^ kind ] ~style:[ ("width", Printf.sprintf "%.2f%%" (100. *. frac)) ] "div" [] ])

(* -- booleans ------------------------------------------------------------- *)

let checkbox ?label ?(disabled = false) v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "change") (fun s -> Var.set v (s = Some "true")) in
    el ~cls:[ "xui-row"; "xui-check" ] "label"
      [
        el "input" ~attrs:([ ("type", "checkbox") ] @ (if Var.get v then [ ("checked", "") ] else []) @ dis disabled)
          ~handlers:[ ("change", h) ] [];
        txt (Option.value label ~default:"");
      ]

let switch ?label v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "change") (fun s -> Var.set v (s = Some "true")) in
    el ~cls:[ "xui-row"; "xui-check" ] "label"
      [
        el "input" ~cls:[ "xui-switch" ] ~attrs:([ ("type", "checkbox"); ("role", "switch") ] @ if Var.get v then [ ("checked", "") ] else [])
          ~handlers:[ ("change", h) ] [];
        txt (Option.value label ~default:"");
      ]

let toggle_button ?(icon = "") text v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "click") (fun _ -> Var.update v not) in
    let on_ = Var.get v in
    el "button" ~cls:([ "xui-button"; "xui-toggle" ] @ if on_ then [ "xui-on" ] else [])
      ~attrs:[ ("aria-pressed", string_of_bool on_) ] ~handlers:[ ("click", h) ]
      [ txt (if icon = "" then text else icon ^ " " ^ text) ]

let valid ?(yes = "valid") ?(no = "invalid") v : t =
 fun _ ->
  let ok = Var.get v in
  el ~cls:[ "xui-valid"; (if ok then "xui-good" else "xui-bad") ] "span" [ txt ((if ok then "✓ " else "✗ ") ^ if ok then yes else no) ]

(* -- choices -------------------------------------------------------------- *)

let index_of options x =
  let rec go i = function
    | [] -> -1
    | (_, y) :: rest -> if same y x then i else go (i + 1) rest
  in
  go 0 options

let pick options v s =
  match Option.bind s int_of_string_opt with
  | Some i when i >= 0 && i < List.length options -> Var.set v (snd (List.nth options i))
  | _ -> ()

let dropdown ?label options v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "change") (pick options v) in
    let sel = index_of options (Var.get v) in
    row ?label
      (el "select" ~cls:[ "xui-input" ] ~attrs:[ ("value", string_of_int sel) ] ~handlers:[ ("change", h) ]
         (List.mapi
            (fun i (name, _) ->
              el "option" ~attrs:([ ("value", string_of_int i) ] @ if i = sel then [ ("selected", "") ] else []) [ txt name ])
            options))

let radio ?label options v : t =
  let uid = fresh () in
  fun ctx ->
    let sel = index_of options (Var.get v) in
    let group = Printf.sprintf "%s-%d" ctx.root uid in
    row ?label
      (el ~cls:[ "xui-radios" ] "div"
         (List.mapi
            (fun i (name, x) ->
              let h = on ctx (key uid (string_of_int i)) (fun s -> if s = Some "true" then Var.set v x) in
              el "label" ~cls:[ "xui-check" ]
                [
                  el "input" ~attrs:([ ("type", "radio"); ("name", group) ] @ if i = sel then [ ("checked", "") ] else [])
                    ~handlers:[ ("change", h) ] [];
                  txt name;
                ])
            options))

let toggle_buttons ?label options v : t =
  let uid = fresh () in
  fun ctx ->
    let cur = Var.get v in
    row ?label
      (el ~cls:[ "xui-segmented" ] "div"
         (List.mapi
            (fun i (name, x) ->
              let h = on ctx (key uid (string_of_int i)) (fun _ -> Var.set v x) in
              el "button" ~cls:([ "xui-segment" ] @ if same x cur then [ "xui-on" ] else [])
                ~attrs:[ ("aria-pressed", string_of_bool (same x cur)) ] ~handlers:[ ("click", h) ] [ txt name ])
            options))

let select_multiple ?label options v : t =
  let uid = fresh () in
  fun ctx ->
    let cur = Var.get v in
    row ?label
      (el ~cls:[ "xui-chips" ] "div"
         (List.mapi
            (fun i (name, x) ->
              let mem x l = List.exists (same x) l in
              let chosen = mem x cur in
              let h =
                on ctx (key uid (string_of_int i)) (fun _ ->
                    let cur = Var.peek v in
                    (* In the options' order, whatever order they were picked in. *)
                    let now = if mem x cur then List.filter (fun y -> not (same x y)) cur else x :: cur in
                    Var.set v (List.filter_map (fun (_, y) -> if mem y now then Some y else None) options))
              in
              el "button" ~cls:([ "xui-chip" ] @ if chosen then [ "xui-on" ] else [])
                ~attrs:[ ("aria-pressed", string_of_bool chosen) ] ~handlers:[ ("click", h) ] [ txt name ])
            options))

let selection_slider ?label options v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (pick options v) in
    let sel = index_of options (Var.get v) in
    row ?label ~readout:(if sel >= 0 then fst (List.nth options sel) else "")
      (el "input" ~cls:[ "xui-slider" ] ~style:[ fill 0. (float (List.length options - 1)) (float sel) ]
         ~attrs:[ ("type", "range"); ("min", "0"); ("max", string_of_int (List.length options - 1)); ("step", "1");
                  ("value", string_of_int sel) ]
         ~handlers:[ ("input", h) ] [])

(* -- text ----------------------------------------------------------------- *)

let text_like ~tag ~typ ?label ?(placeholder = "") ?on_submit ?(attrs = []) v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s -> Var.set v (Option.value s ~default:"")) in
    let submit =
      match on_submit with
      | Some f -> [ ("change", on ctx (key uid "change") (fun s -> f (Option.value s ~default:""))) ]
      | None -> []
    in
    row ?label
      (el tag ~cls:[ "xui-input" ]
         ~attrs:((if typ = "" then [] else [ ("type", typ) ]) @ [ ("placeholder", placeholder); ("value", Var.get v) ] @ attrs)
         ~handlers:(("input", h) :: submit) [])

let text_input ?label ?placeholder ?on_submit v = text_like ~tag:"input" ~typ:"text" ?label ?placeholder ?on_submit v
let password ?label ?placeholder v = text_like ~tag:"input" ~typ:"password" ?label ?placeholder v

let textarea ?label ?placeholder ?(rows = 4) v =
  text_like ~tag:"textarea" ~typ:"" ?label ?placeholder ~attrs:[ ("rows", string_of_int rows) ] v

let color_picker ?label v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s -> Option.iter (Var.set v) s) in
    row ?label ~readout:(Var.get v)
      (el "input" ~cls:[ "xui-color" ] ~attrs:[ ("type", "color"); ("value", Var.get v) ] ~handlers:[ ("input", h) ] [])

let date_like typ ?label v : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "input") (fun s -> Option.iter (Var.set v) s) in
    row ?label (el "input" ~cls:[ "xui-input" ] ~attrs:[ ("type", typ); ("value", Var.get v) ] ~handlers:[ ("input", h) ] [])

let date_picker ?label v = date_like "date" ?label v
let time_picker ?label v = date_like "time" ?label v

let combobox ?label ?placeholder ~options v : t =
  let uid = fresh () in
  fun ctx ->
    let list = Printf.sprintf "%s-%d-list" ctx.root uid in
    let input = text_like ~tag:"input" ~typ:"text" ?label ?placeholder ~attrs:[ ("list", list) ] v ctx in
    el "div" [ input; el "datalist" ~attrs:[ ("id", list) ] (List.map (fun o -> el "option" ~attrs:[ ("value", o) ] []) options) ]

let tags ?label ?(placeholder = "add a tag, then Enter") v : t =
  let uid = fresh () in
  fun ctx ->
    let add s =
      match String.trim (Option.value s ~default:"") with
      | "" -> ()
      | t -> if not (List.mem t (Var.peek v)) then Var.set v (Var.peek v @ [ t ])
    in
    let h = on ctx (key uid "add") add in
    row ?label
      (el ~cls:[ "xui-tags" ] "div"
         (List.mapi
            (fun i t ->
              let rm = on ctx (key uid ("rm" ^ string_of_int i)) (fun _ -> Var.set v (List.filter (( <> ) t) (Var.peek v))) in
              el ~cls:[ "xui-tag" ] "span" [ txt t; el "button" ~attrs:[ ("aria-label", "remove " ^ t) ] ~handlers:[ ("click", rm) ] [ txt "×" ] ])
            (Var.get v)
         @ [ el "input" ~cls:[ "xui-tag-input" ] ~attrs:[ ("placeholder", placeholder); ("value", "") ] ~handlers:[ ("change", h) ] [] ]))

(* -- actions -------------------------------------------------------------- *)

let button ?(kind = "plain") ?(icon = "") ?(disabled = false) text f : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "click") (fun _ -> f ()) in
    el "button" ~cls:[ "xui-button"; "xui-" ^ kind ] ~attrs:(dis disabled) ~handlers:[ ("click", h) ]
      [ txt (if icon = "" then text else if text = "" then icon else icon ^ " " ^ text) ]

(* A timer, drawn by the page: it ticks only once the worker has handled the
   tick before, so a slow handler slows it rather than piling ticks up. *)
let clock_adapter =
  {|(function () {
  function apply(s, c) {
    s.c = c;
    if (s.timer) clearInterval(s.timer);
    s.timer = null;
    if (c.running) s.timer = setInterval(function () {
      if (s.c.done >= s.sent) { s.sent = s.c.done + 1; s.send(s.c.handler, String(s.sent)); }
    }, Math.max(16, c.interval));
  }
  return {
    create: function (el, config, send) {
      var c = JSON.parse(config), s = { send: send, sent: c.done };
      apply(s, c);
      return s;
    },
    update: function (s, config) {
      var c = JSON.parse(config);
      // A new handler is a new clock, whose ticks start from its own count.
      if (c.handler !== s.c.handler) s.sent = c.done;
      if (c.running !== s.c.running || c.interval !== s.c.interval || c.handler !== s.c.handler) apply(s, c);
      else s.c = c;
    },
    command: function () {},
    destroy: function (s) { if (s.timer) clearInterval(s.timer); }
  };
})()|}

let adapters = Hashtbl.create 4

let adapter kind js =
  if not (Hashtbl.mem adapters kind) then begin
    Hashtbl.replace adapters kind ();
    Widget.register_adapter ~kind ~js
  end

let clock ?(interval = 100) ?every ~running f : t =
  let uid = fresh () in
  let done_ = Var.create 0 in
  fun ctx ->
    adapter "x-ocaml-ui-clock" clock_adapter;
    let h = on ctx (key uid "tick") (fun _ -> Var.update done_ succ; f ()) in
    Managed
      {
        kind = "x-ocaml-ui-clock";
        config =
          Printf.sprintf {|{"running":%b,"interval":%d,"handler":%s,"done":%d}|} (Var.get running)
            (match every with Some v -> Var.get v | None -> interval)
            (Json.str h)
            (Var.get done_);
      }

let play ?label ?(interval = 100) ?every ?(loop = true) ?(min = 0) ~max v : t =
  let running = Var.create false in
  let tick () =
    let n = Var.peek v + 1 in
    if n <= max then Var.set v n
    else if loop then Var.set v min
    else Var.set running false
  in
  let ticker = clock ~interval ?every ~running tick in
  let play_btn = dyn (fun () ->
      if Var.get running then button ~icon:"⏸" "" (fun () -> Var.set running false)
      else button ~icon:"▶" "" (fun () -> Var.set running true)) in
  let slider = int_slider ~min ~max v in
  let bits =
    [
      play_btn;
      button ~icon:"⏹" "" (fun () -> Var.set running false; Var.set v min);
      button ~icon:"⏭" "" tick;
    ]
  in
  fun ctx ->
    row ?label
      (el ~cls:[ "xui-play" ] "div"
         (List.map (fun b -> b ctx) bits @ [ el ~cls:[ "xui-grow" ] "div" [ slider ctx ]; ticker ctx ]))

let keyboard ?(prevent = true) ~on_key (w : t) : t =
  let uid = fresh () in
  fun ctx ->
    let h = on ctx (key uid "keydown") (fun v -> on_key (Event.of_value v)) in
    el "div" ~cls:[ "xui-keys" ]
      ~attrs:([ ("tabindex", "0") ] @ if prevent then [ ("data-x-ocaml-prevent", "keydown") ] else [])
      ~handlers:[ ("keydown", h) ] [ w ctx ]

(* -- output --------------------------------------------------------------- *)

module Output = struct
  type t = { lines : string list Var.t; limit : int }

  let create ?(limit = 200) () = { lines = Var.create []; limit }

  let print o s =
    let lines = String.split_on_char '\n' s in
    Var.update o.lines (fun old ->
        let all = List.rev_append lines old in
        List.filteri (fun i _ -> i < o.limit) all)

  let printf o fmt = Printf.ksprintf (print o) fmt
  let clear o = Var.set o.lines []
end

let output ?(height = "12rem") (o : Output.t) : t =
 fun _ ->
  el ~cls:[ "xui-output" ] ~style:[ ("max-height", height) ] "pre" [ txt (String.concat "\n" (List.rev (Var.get o.lines))) ]

(* -- layout --------------------------------------------------------------- *)

let box cls ?(gap = "0.75rem") ?(style = []) ws : t =
 fun ctx -> el ~cls ~style:(("gap", gap) :: style) "div" (List.map (fun w -> w ctx) ws)

let hbox ?gap ?(align = "center") ?(wrap = true) ws =
  box [ "xui-hbox" ] ?gap ~style:[ ("align-items", align); ("flex-wrap", if wrap then "wrap" else "nowrap") ] ws

let vbox ?gap ws = box [ "xui-vbox" ] ?gap ws

let grid ?gap ?(columns = 2) ws =
  box [ "xui-grid" ] ?gap ~style:[ ("grid-template-columns", Printf.sprintf "repeat(%d, minmax(0, 1fr))" columns) ] ws

let card ?title ws : t =
 fun ctx ->
  el ~cls:[ "xui-card" ] "div"
    ((match title with Some t -> [ el ~cls:[ "xui-card-title" ] "div" [ txt t ] ] | None -> [])
    @ List.map (fun w -> w ctx) ws)

let styled style (w : t) : t = fun ctx -> el ~style "div" [ w ctx ]

let tabs pages : t =
  let uid = fresh () in
  let sel = Var.create 0 in
  fun ctx ->
    let cur = Var.get sel in
    el ~cls:[ "xui-tabs" ] "div"
      [
        el ~cls:[ "xui-tab-bar" ] ~attrs:[ ("role", "tablist") ] "div"
          (List.mapi
             (fun i (name, _) ->
               let h = on ctx (key uid (string_of_int i)) (fun _ -> Var.set sel i) in
               el "button" ~cls:([ "xui-tab" ] @ if i = cur then [ "xui-on" ] else [])
                 ~attrs:[ ("role", "tab"); ("aria-selected", string_of_bool (i = cur)) ] ~handlers:[ ("click", h) ] [ txt name ])
             pages);
        el ~cls:[ "xui-tab-panel" ] ~attrs:[ ("role", "tabpanel") ] "div"
          [ (match List.nth_opt pages cur with Some (_, w) -> w ctx | None -> txt "") ];
      ]

let accordion ?(open_ = []) sections : t =
 fun ctx ->
  el ~cls:[ "xui-accordion" ] "div"
    (List.mapi
       (fun i (name, w) ->
         el "details" ~attrs:(if List.mem i open_ then [ ("open", "") ] else []) [ el "summary" [ txt name ]; el "div" [ w ctx ] ])
       sections)

(* -- tables --------------------------------------------------------------- *)

let table ?(page_size = 10) ~header rows : t =
  let uid = fresh () in
  let sort = Var.create None and filter = Var.create "" and page = Var.create 0 in
  let search = text_input ~placeholder:"filter" filter in
  fun ctx ->
    let all = rows () in
    let q = String.lowercase_ascii (Var.get filter) in
    let contains s =
      let s = String.lowercase_ascii s and n = String.length q in
      let rec go i = i + n <= String.length s && (String.sub s i n = q || go (i + 1)) in
      n = 0 || go 0
    in
    let shown = List.filter (List.exists contains) all in
    let cmp a b =
      match (float_of_string_opt a, float_of_string_opt b) with
      | Some x, Some y -> compare x y
      | _ -> compare (String.lowercase_ascii a) (String.lowercase_ascii b)
    in
    let shown =
      match Var.get sort with
      | Some (col, asc) ->
          let nth r = Option.value (List.nth_opt r col) ~default:"" in
          List.stable_sort (fun a b -> let c = cmp (nth a) (nth b) in if asc then c else -c) shown
      | None -> shown
    in
    let n = List.length shown in
    let pages = Stdlib.max 1 ((n + page_size - 1) / page_size) in
    let p = Stdlib.min (Var.get page) (pages - 1) in
    let visible = List.filteri (fun i _ -> i >= p * page_size && i < (p + 1) * page_size) shown in
    let th i name =
      let h = on ctx (key uid ("sort" ^ string_of_int i)) (fun _ ->
          Var.set sort (match Var.peek sort with Some (c, true) when c = i -> Some (i, false) | Some (c, false) when c = i -> None | _ -> Some (i, true)))
      in
      let arrow = match Var.get sort with Some (c, a) when c = i -> if a then " ▲" else " ▼" | _ -> "" in
      el "th" ~handlers:[ ("click", h) ] ~attrs:[ ("aria-sort", match Var.get sort with Some (c, a) when c = i -> if a then "ascending" else "descending" | _ -> "none") ] [ txt (name ^ arrow) ]
    in
    let nav name d enabled =
      let h = on ctx (key uid name) (fun _ -> Var.set page (Stdlib.max 0 (Stdlib.min (pages - 1) (p + d)))) in
      el "button" ~cls:[ "xui-button"; "xui-plain" ] ~attrs:(dis (not enabled)) ~handlers:[ ("click", h) ] [ txt name ]
    in
    el ~cls:[ "xui-table-wrap" ] "div"
      [
        el ~cls:[ "xui-table-tools" ] "div"
          [ search ctx; el ~cls:[ "xui-readout" ] "span" [ txt (Printf.sprintf "%d of %d rows" n (List.length all)) ] ];
        el ~cls:[ "xui-table" ] "table"
          [
            el "thead" [ el "tr" (List.mapi th header) ];
            el "tbody" (List.map (fun r -> el "tr" (List.map (fun c -> el "td" ~cls:(if float_of_string_opt c <> None then [ "xui-num" ] else []) [ txt c ]) r)) visible);
          ];
        (if pages > 1 then
           el ~cls:[ "xui-table-tools" ] "div"
             [ nav "‹ prev" (-1) (p > 0); el ~cls:[ "xui-readout" ] "span" [ txt (Printf.sprintf "page %d of %d" (p + 1) pages) ]; nav "next ›" 1 (p < pages - 1) ]
         else txt "");
      ]

(* -- images --------------------------------------------------------------- *)

module Image = struct
  type t = { width : int; height : int; url : string }

  let base64 s =
    let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
    let n = String.length s in
    let b = Buffer.create ((n + 2) / 3 * 4) in
    let byte i = if i < n then Char.code s.[i] else 0 in
    let i = ref 0 in
    while !i < n do
      let x = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
      Buffer.add_char b tbl.[(x lsr 18) land 63];
      Buffer.add_char b tbl.[(x lsr 12) land 63];
      Buffer.add_char b (if !i + 1 < n then tbl.[(x lsr 6) land 63] else '=');
      Buffer.add_char b (if !i + 2 < n then tbl.[x land 63] else '=');
      i := !i + 3
    done;
    Buffer.contents b

  (* A 24-bit BMP: the simplest format a browser shows. *)
  let rgb ~width ~height f =
    let stride = (width * 3 + 3) land lnot 3 in
    let size = 54 + (stride * height) in
    let b = Bytes.make size '\000' in
    let u16 o v = Bytes.set_uint16_le b o v and u32 o v = Bytes.set_int32_le b o (Int32.of_int v) in
    Bytes.set b 0 'B';
    Bytes.set b 1 'M';
    u32 2 size;
    u32 10 54;
    u32 14 40;
    u32 18 width;
    u32 22 (-height) (* top-down rows *);
    u16 26 1;
    u16 28 24;
    u32 34 (stride * height);
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let r, g, bl = f x y in
        let o = 54 + (y * stride) + (x * 3) in
        let c v = Char.unsafe_chr (Stdlib.max 0 (Stdlib.min 255 v)) in
        Bytes.set b o (c bl);
        Bytes.set b (o + 1) (c g);
        Bytes.set b (o + 2) (c r)
      done
    done;
    { width; height; url = "data:image/bmp;base64," ^ base64 (Bytes.unsafe_to_string b) }

  let of_url ~width ~height url = { width; height; url }

  (* Hue in turns, saturation and value from 0 to 1. *)
  let hsv h s v =
    let h = (h -. floor h) *. 6. in
    let i = int_of_float h and f = h -. floor h in
    let p = v *. (1. -. s) and q = v *. (1. -. (s *. f)) and t = v *. (1. -. (s *. (1. -. f))) in
    let r, g, b =
      match i with 0 -> (v, t, p) | 1 -> (q, v, p) | 2 -> (p, v, t) | 3 -> (p, q, v) | 4 -> (t, p, v) | _ -> (v, p, q)
    in
    let c x = int_of_float (x *. 255.) in
    (c r, c g, c b)
end

let image ?(scale = 1.) ?(smooth = false) ?on_click ?alt (img : Image.t) : t =
  let uid = fresh () in
  fun ctx ->
    let handlers =
      match on_click with
      | Some f ->
          [ ("click", on ctx (key uid "click") (fun v ->
                let e = Event.of_value v in
                let px = e.x /. Float.max 1. e.width *. float img.width
                and py = e.y /. Float.max 1. e.height *. float img.height in
                f ~shift:e.shift (int_of_float px) (int_of_float py))) ]
      | None -> []
    in
    el "img" ~cls:([ "xui-image" ] @ (if smooth then [ "xui-smooth" ] else []) @ if on_click <> None then [ "xui-clickable" ] else [])
      ~attrs:[ ("src", img.url); ("alt", Option.value alt ~default:""); ("width", string_of_int (int_of_float (float img.width *. scale))) ]
      ~handlers []

(* -- plots ---------------------------------------------------------------- *)

module Plot = struct
  type mark =
    | Line of { pts : (float * float) list; width : float; dash : bool }
    | Area of (float * float) list
    | Scatter of { pts : (float * float) list; size : float; tips : string list }
    | Bars of { pts : (float * float) list; bar : float option }
    | Fn of { f : float -> float; samples : int }
    | Hline of float
    | Vline of float
    | Label of float * float * string

  type series = { mark : mark; label : string option; color : string option }

  let palette = [| "#e8661d"; "#2f6fdb"; "#1f9d6a"; "#9b51e0"; "#d63d3d"; "#c99a06"; "#1497a8"; "#6c757d" |]
  let s ?label ?color mark = { mark; label; color }
  let line ?label ?color ?(width = 2.) ?(dash = false) pts = s ?label ?color (Line { pts; width; dash })
  let area ?label ?color pts = s ?label ?color (Area pts)
  let scatter ?label ?color ?(size = 4.) ?(tips = []) pts = s ?label ?color (Scatter { pts; size; tips })
  let bars ?label ?color ?width pts = s ?label ?color (Bars { pts; bar = width })
  let fn ?label ?color ?(samples = 200) f = s ?label ?color (Fn { f; samples })
  let hline ?color y = s ?color (Hline y)
  let vline ?color x = s ?color (Vline x)
  let text ?color (x, y) str = s ?color (Label (x, y, str))

  let histogram ?label ?color ?(bins = 20) data =
    match data with
    | [] -> bars ?label ?color []
    | d :: _ ->
        let lo = List.fold_left Float.min d data and hi = List.fold_left Float.max d data in
        let hi = if hi = lo then lo +. 1. else hi in
        let w = (hi -. lo) /. float bins in
        let counts = Array.make bins 0 in
        List.iter (fun x -> let i = Stdlib.min (bins - 1) (int_of_float ((x -. lo) /. w)) in counts.(i) <- counts.(i) + 1) data;
        bars ?label ?color ~width:w (List.init bins (fun i -> (lo +. ((float i +. 0.5) *. w), float counts.(i))))

  type pointer = { x : float; y : float; shift : bool; alt : bool; buttons : int }

  (* Ticks at 1, 2 or 5 times a power of ten. *)
  let ticks lo hi n =
    let span = hi -. lo in
    if not (Float.is_finite span) || span <= 0. then []
    else
      let raw = span /. float n in
      let mag = 10. ** floor (log10 raw) in
      let step = List.find (fun m -> m *. mag >= raw) [ 1.; 2.; 5.; 10. ] *. mag in
      let first = ceil (lo /. step) *. step in
      let rec go x acc = if x > hi +. (step *. 1e-9) then List.rev acc else go (x +. step) (x :: acc) in
      go first []

  let fmt_tick x =
    let s = Printf.sprintf "%.6g" (if Float.abs x < 1e-12 then 0. else x) in
    s

  let f1 x = Printf.sprintf "%.2f" x

  let points_of = function
    | Line { pts; _ } | Area pts | Scatter { pts; _ } | Bars { pts; _ } -> pts
    | Label (x, y, _) -> [ (x, y) ]
    | Fn _ | Hline _ | Vline _ -> []

  let finite (x, y) = Float.is_finite x && Float.is_finite y

  let extent pad values default =
    match List.filter Float.is_finite values with
    | [] -> default
    | v :: vs ->
        let lo = List.fold_left Float.min v vs and hi = List.fold_left Float.max v vs in
        if hi = lo then (lo -. 1., hi +. 1.) else let p = (hi -. lo) *. pad in (lo -. p, hi +. p)

  let chart ?title ?x_label ?y_label ?(width = 640) ?(height = 360) ?x_range ?y_range ?(legend = true) ?(grid = true)
      ?on_click ?on_press ?on_drag ?on_release ?on_move ?(aspect_equal = false) (series : series list) : t =
    let uid = fresh () in
    fun ctx ->
      let w = float width and h = float height in
      let ml = 56. and mr = 18. and mt = if title = None then 14. else 36. and mb = if x_label = None then 30. else 48. in
      let pw = w -. ml -. mr and ph = h -. mt -. mb in
      let pts = List.concat_map (fun s -> List.filter finite (points_of s.mark)) series in
      let has_bars = List.exists (fun s -> match s.mark with Bars _ | Area _ -> true | _ -> false) series in
      let x0, x1 = match x_range with Some r -> r | None -> extent 0.03 (List.map fst pts) (0., 1.) in
      (* Bars stand on zero, and the tallest keeps a little headroom. *)
      let fn_ys =
        List.concat_map (fun s -> match s.mark with
            | Fn { f; samples } -> List.init (samples + 1) (fun i -> f (x0 +. ((x1 -. x0) *. float i /. float samples)))
            | Hline y -> [ y ]
            | _ -> []) series
      in
      let y0, y1 =
        match y_range with
        | Some r -> r
        | None ->
            let ys = List.map snd pts @ fn_ys @ if has_bars then [ 0. ] else [] in
            extent 0.06 ys (0., 1.)
      in
      let y0, y1, x0, x1 =
        if aspect_equal && x_range = None then
          let sx = (x1 -. x0) /. pw and sy = (y1 -. y0) /. ph in
          if sx > sy then let c = (y0 +. y1) /. 2. and hh = sx *. ph /. 2. in (c -. hh, c +. hh, x0, x1)
          else let c = (x0 +. x1) /. 2. and hw = sy *. pw /. 2. in (y0, y1, c -. hw, c +. hw)
        else (y0, y1, x0, x1)
      in
      let sx x = ml +. ((x -. x0) /. (x1 -. x0) *. pw) and sy y = mt +. ph -. ((y -. y0) /. (y1 -. y0) *. ph) in
      let color i s = match s.color with Some c -> c | None -> palette.(i mod Array.length palette) in
      let clip = Printf.sprintf "%s-clip-%d" ctx.root uid in
      let a k v = (k, v) in
      let line_el ?(cls = []) ?(extra = []) x1_ y1_ x2 y2 = el ~cls "line" ~attrs:([ a "x1" (f1 x1_); a "y1" (f1 y1_); a "x2" (f1 x2); a "y2" (f1 y2) ] @ extra) [] in
      let xt = ticks x0 x1 (Stdlib.max 2 (width / 90)) and yt = ticks y0 y1 (Stdlib.max 2 (height / 60)) in
      let grid_els =
        if not grid then []
        else
          List.map (fun x -> line_el ~cls:[ "xui-grid-line" ] (sx x) mt (sx x) (mt +. ph)) xt
          @ List.map (fun y -> line_el ~cls:[ "xui-grid-line" ] ml (sy y) (ml +. pw) (sy y)) yt
      in
      let axis_els =
        [ line_el ~cls:[ "xui-axis" ] ml (mt +. ph) (ml +. pw) (mt +. ph); line_el ~cls:[ "xui-axis" ] ml mt ml (mt +. ph) ]
        @ List.map (fun x -> el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (sx x)); a "y" (f1 (mt +. ph +. 16.)); a "text-anchor" "middle" ] [ txt (fmt_tick x) ]) xt
        @ List.map (fun y -> el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (ml -. 7.)); a "y" (f1 (sy y +. 4.)); a "text-anchor" "end" ] [ txt (fmt_tick y) ]) yt
        @ (match x_label with Some l -> [ el "text" ~cls:[ "xui-axis-label" ] ~attrs:[ a "x" (f1 (ml +. (pw /. 2.))); a "y" (f1 (h -. 10.)); a "text-anchor" "middle" ] [ txt l ] ] | None -> [])
        @ (match y_label with
           | Some l -> [ el "text" ~cls:[ "xui-axis-label" ] ~attrs:[ a "transform" (Printf.sprintf "translate(14 %s) rotate(-90)" (f1 (mt +. (ph /. 2.)))); a "text-anchor" "middle" ] [ txt l ] ]
           | None -> [])
        @ match title with Some t -> [ el "text" ~cls:[ "xui-chart-title" ] ~attrs:[ a "x" (f1 ml); a "y" "22" ] [ txt t ] ] | None -> []
      in
      (* Lines break where the data is not finite. *)
      let path pts =
        let buf = Buffer.create 256 in
        let pen = ref false in
        List.iter (fun (x, y) ->
            if finite (x, y) then begin
              Buffer.add_string buf (Printf.sprintf "%s%s,%s" (if !pen then "L" else "M") (f1 (sx x)) (f1 (sy (Float.max (y0 -. (y1 -. y0)) (Float.min (y1 +. (y1 -. y0)) y)))));
              pen := true
            end
            else pen := false) pts;
        Buffer.contents buf
      in
      let mark_els i s =
        let c = color i s in
        match s.mark with
        | Line { pts; width; dash } ->
            [ el "path" ~cls:[ "xui-line" ] ~attrs:([ a "d" (path pts); a "stroke" c; a "stroke-width" (f1 width); a "fill" "none" ] @ if dash then [ a "stroke-dasharray" "6 4" ] else []) [] ]
        | Fn { f; samples } ->
            let pts = List.init (samples + 1) (fun k -> let x = x0 +. ((x1 -. x0) *. float k /. float samples) in (x, f x)) in
            [ el "path" ~cls:[ "xui-line" ] ~attrs:[ a "d" (path pts); a "stroke" c; a "stroke-width" "2"; a "fill" "none" ] [] ]
        | Area pts -> (
            match List.filter finite pts with
            | [] -> []
            | (fx, _) :: _ as fp ->
                let lx = fst (List.nth fp (List.length fp - 1)) in
                let base = sy (Float.max y0 (Float.min y1 0.)) in
                [ el "path" ~attrs:[ a "d" (Printf.sprintf "%sL%s,%sL%s,%sZ" (path fp) (f1 (sx lx)) (f1 base) (f1 (sx fx)) (f1 base)); a "fill" c; a "fill-opacity" "0.25"; a "stroke" c; a "stroke-width" "1.5" ] [] ])
        | Scatter { pts; size; tips } ->
            List.mapi (fun k (x, y) ->
                if not (finite (x, y)) then txt ""
                else
                  let tip = match List.nth_opt tips k with Some t -> t | None -> Printf.sprintf "(%s, %s)" (fmt_tick x) (fmt_tick y) in
                  el "circle" ~cls:[ "xui-dot" ] ~attrs:[ a "cx" (f1 (sx x)); a "cy" (f1 (sy y)); a "r" (f1 size); a "fill" c ] [ el "title" [ txt tip ] ]) pts
        | Bars { pts; bar } ->
            let bw =
              match bar with
              | Some b -> b
              | None ->
                  let xs = List.sort_uniq compare (List.map fst pts) in
                  let rec gap m = function a :: (b :: _ as r) -> gap (Float.min m (b -. a)) r | _ -> m in
                  (match xs with _ :: _ :: _ -> gap infinity xs | _ -> (x1 -. x0) /. 10.) *. 0.8
            in
            let base = sy (Float.max y0 (Float.min y1 0.)) in
            List.map (fun (x, y) ->
                let top = sy y in
                let px = sx (x -. (bw /. 2.)) and pw_ = (sx (x +. (bw /. 2.))) -. sx (x -. (bw /. 2.)) in
                el "rect" ~cls:[ "xui-bar" ] ~attrs:[ a "x" (f1 px); a "y" (f1 (Float.min top base)); a "width" (f1 (Float.max 0.5 (pw_ -. 1.))); a "height" (f1 (Float.abs (base -. top))); a "fill" c ]
                  [ el "title" [ txt (Printf.sprintf "%s: %s" (fmt_tick x) (fmt_tick y)) ] ]) pts
        | Hline y -> [ line_el ~cls:[ "xui-rule" ] ~extra:[ a "stroke" c ] ml (sy y) (ml +. pw) (sy y) ]
        | Vline x -> [ line_el ~cls:[ "xui-rule" ] ~extra:[ a "stroke" c ] (sx x) mt (sx x) (mt +. ph) ]
        | Label (x, y, str) -> [ el "text" ~cls:[ "xui-annot" ] ~attrs:[ a "x" (f1 (sx x)); a "y" (f1 (sy y)); a "fill" c ] [ txt str ] ]
      in
      let labelled = List.filter_map (fun (i, s) -> Option.map (fun l -> (color i s, l)) s.label) (List.mapi (fun i s -> (i, s)) series) in
      let legend_els =
        if (not legend) || labelled = [] then []
        else
          let lh = 18. in
          let lw = 20. +. (7.2 *. float (List.fold_left (fun m (_, l) -> Stdlib.max m (String.length l)) 0 labelled)) +. 16. in
          [ el "g" ~cls:[ "xui-legend" ] ~attrs:[ a "transform" (Printf.sprintf "translate(%s %s)" (f1 (ml +. pw -. lw -. 6.)) (f1 (mt +. 6.))) ]
              (el "rect" ~attrs:[ a "width" (f1 lw); a "height" (f1 ((lh *. float (List.length labelled)) +. 8.)); a "rx" "4" ] []
              :: List.concat (List.mapi (fun k (c, l) ->
                     let y = 4. +. (lh *. float k) +. (lh /. 2.) in
                     [ el "rect" ~attrs:[ a "x" "8"; a "y" (f1 (y -. 4.)); a "width" "12"; a "height" "8"; a "rx" "2"; a "fill" c ] [];
                       el "text" ~attrs:[ a "x" "26"; a "y" (f1 (y +. 4.)) ] [ txt l ] ]) labelled)) ]
      in
      let to_data (e : Event.t) = { x = x0 +. ((e.x -. ml) /. pw *. (x1 -. x0)); y = y0 +. ((mt +. ph -. e.y) /. ph *. (y1 -. y0)); shift = e.shift; alt = e.alt; buttons = e.buttons } in
      let inside (e : Event.t) = e.x >= ml && e.x <= ml +. pw && e.y >= mt && e.y <= mt +. ph in
      let handler ?(anywhere = false) name f =
        on ctx (key uid name) (fun v -> let e = Event.of_value v in if anywhere || inside e then f (to_data e))
      in
      let opt name ev ?anywhere f = match f with Some f -> [ (ev, handler ?anywhere name f) ] | None -> [] in
      let move =
        match (on_drag, on_move) with
        | None, None -> []
        | _ ->
            [ ("pointermove", handler "move" (fun p ->
                  match (on_drag, on_move) with
                  | Some f, _ when p.buttons land 1 = 1 -> f p
                  | _, Some g when p.buttons = 0 -> g p
                  | _ -> ())) ]
      in
      let handlers =
        opt "click" "click" on_click @ opt "press" "pointerdown" on_press @ move
        @ opt "release" "pointerup" ~anywhere:true on_release
      in
      el "svg" ~cls:([ "xui-chart" ] @ if handlers <> [] then [ "xui-interactive" ] else [])
        ~attrs:([ a "viewBox" (Printf.sprintf "0 0 %d %d" width height); a "preserveAspectRatio" "xMidYMid meet" ]
               @ if on_drag <> None || on_press <> None then [ a "data-x-ocaml-prevent" "pointerdown" ] else [])
        ~handlers
        ([ el "defs" [ el "clipPath" ~attrs:[ a "id" clip ] [ el "rect" ~attrs:[ a "x" (f1 ml); a "y" (f1 mt); a "width" (f1 pw); a "height" (f1 ph) ] [] ] ];
           el "rect" ~cls:[ "xui-plot-bg" ] ~attrs:[ a "x" (f1 ml); a "y" (f1 mt); a "width" (f1 pw); a "height" (f1 ph) ] [] ]
        @ grid_els @ axis_els
        @ [ el "g" ~attrs:[ a "clip-path" (Printf.sprintf "url(#%s)" clip) ] (List.concat (List.mapi mark_els series)) ]
        @ legend_els)

  let bar_chart ?title ?(horizontal = false) ?color ?(width = 640) ?height ?on_click ?(format = fmt_tick) (data : (string * float) list) : t =
    let uid = fresh () in
    fun ctx ->
      let n = List.length data in
      let height = match height with Some h -> h | None -> if horizontal then 40 + (n * 30) else 320 in
      let w = float width and h = float height in
      let mt = if title = None then 12. else 34. in
      let vmax = List.fold_left (fun m (_, v) -> Float.max m v) 0. data in
      let vmax = if vmax <= 0. then 1. else vmax in
      let a k v = (k, v) in
      let label_w = if horizontal then 12. +. (7. *. float (List.fold_left (fun m (l, _) -> Stdlib.max m (String.length l)) 0 data)) else 0. in
      let bar i (name, v) =
        let c = match color with Some c -> c | None -> palette.(i mod Array.length palette) in
        let handlers = match on_click with Some f -> [ ("click", on ctx (key uid (string_of_int i)) (fun _ -> f i)) ] | None -> [] in
        let rect, value_label, name_label =
          if horizontal then
            let bh = (h -. mt -. 10.) /. float n in
            let y = mt +. (bh *. float i) +. (bh *. 0.15) in
            let len = (w -. label_w -. 60.) *. (v /. vmax) in
            ( el "rect" ~cls:[ "xui-bar" ] ~attrs:[ a "x" (f1 label_w); a "y" (f1 y); a "width" (f1 (Float.max 0. len)); a "height" (f1 (bh *. 0.7)); a "rx" "3"; a "fill" c ] ~handlers [ el "title" [ txt (name ^ ": " ^ format v) ] ],
              el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (label_w +. len +. 6.)); a "y" (f1 (y +. (bh *. 0.35) +. 4.)) ] [ txt (format v) ],
              el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (label_w -. 8.)); a "y" (f1 (y +. (bh *. 0.35) +. 4.)); a "text-anchor" "end" ] [ txt name ] )
          else
            let bw = (w -. 40.) /. float n in
            let x = 20. +. (bw *. float i) +. (bw *. 0.15) in
            let len = (h -. mt -. 50.) *. (v /. vmax) in
            let base = h -. 30. in
            ( el "rect" ~cls:[ "xui-bar" ] ~attrs:[ a "x" (f1 x); a "y" (f1 (base -. len)); a "width" (f1 (bw *. 0.7)); a "height" (f1 (Float.max 0. len)); a "rx" "3"; a "fill" c ] ~handlers [ el "title" [ txt (name ^ ": " ^ format v) ] ],
              el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (x +. (bw *. 0.35))); a "y" (f1 (base -. len -. 6.)); a "text-anchor" "middle" ] [ txt (format v) ],
              el "text" ~cls:[ "xui-tick" ] ~attrs:[ a "x" (f1 (x +. (bw *. 0.35))); a "y" (f1 (base +. 16.)); a "text-anchor" "middle" ] [ txt name ] )
        in
        el "g" ~cls:(if on_click <> None then [ "xui-clickable" ] else []) [ rect; value_label; name_label ]
      in
      el "svg" ~cls:[ "xui-chart" ] ~attrs:[ a "viewBox" (Printf.sprintf "0 0 %d %d" width height) ]
        ((match title with Some t -> [ el "text" ~cls:[ "xui-chart-title" ] ~attrs:[ a "x" "8"; a "y" "22" ] [ txt t ] ] | None -> [])
        @ List.mapi bar data)

  let pie ?title ?(donut = true) ?(size = 320) ?on_click (data : (string * float) list) : t =
    let uid = fresh () in
    fun ctx ->
      let total = List.fold_left (fun s (_, v) -> s +. Float.max 0. v) 0. data in
      let total = if total <= 0. then 1. else total in
      let s = float size in
      let cx = s /. 2. and cy = (s /. 2.) +. if title = None then 0. else 14. in
      let r = (s /. 2.) -. 16. and ri = if donut then s /. 4.5 else 0. in
      let a k v = (k, v) in
      let pt rr t = (cx +. (rr *. sin t), cy -. (rr *. cos t)) in
      let _, slices =
        List.fold_left (fun (start, acc) (i, (name, v)) ->
            let sweep = 2. *. Float.pi *. Float.max 0. v /. total in
            let sweep = Float.min sweep (2. *. Float.pi -. 1e-4) in
            let e = start +. sweep in
            let large = if sweep > Float.pi then 1 else 0 in
            let x1, y1 = pt r start and x2, y2 = pt r e and x3, y3 = pt ri e and x4, y4 = pt ri start in
            let d = Printf.sprintf "M%s,%sA%s,%s 0 %d 1 %s,%sL%s,%sA%s,%s 0 %d 0 %s,%sZ" (f1 x1) (f1 y1) (f1 r) (f1 r) large (f1 x2) (f1 y2) (f1 x3) (f1 y3) (f1 ri) (f1 ri) large (f1 x4) (f1 y4) in
            let handlers = match on_click with Some f -> [ ("click", on ctx (key uid (string_of_int i)) (fun _ -> f i)) ] | None -> [] in
            let c = palette.(i mod Array.length palette) in
            let mx, my = pt ((r +. ri) /. 2.) (start +. (sweep /. 2.)) in
            let pct = 100. *. v /. total in
            ( e,
              acc
              @ [ el "g" ~cls:[ "xui-slice" ] ~handlers
                    [ el "path" ~attrs:[ a "d" d; a "fill" c ] [ el "title" [ txt (Printf.sprintf "%s: %s (%.1f%%)" name (fmt_tick v) pct) ] ];
                      (if pct >= 6. then el "text" ~cls:[ "xui-slice-label" ] ~attrs:[ a "x" (f1 mx); a "y" (f1 (my +. 4.)); a "text-anchor" "middle" ] [ txt name ] else txt "") ] ] ))
          (0., []) (List.mapi (fun i d -> (i, d)) data)
      in
      el "svg" ~cls:[ "xui-chart"; "xui-pie" ] ~attrs:[ a "viewBox" (Printf.sprintf "0 0 %d %d" size (size + if title = None then 0 else 28)) ]
        ((match title with Some t -> [ el "text" ~cls:[ "xui-chart-title" ] ~attrs:[ a "x" "8"; a "y" "20" ] [ txt t ] ] | None -> []) @ slices)
end

(* -- interact ------------------------------------------------------------- *)

module Interact = struct
  type 'a control = { controls : t list; value : unit -> 'a }

  let of_var w v = { controls = [ w ]; value = (fun () -> Var.get v) }
  let int ?min ?max ?step label init = let v = Var.create init in of_var (int_slider ~label ?min ?max ?step v) v
  let float ?min ?max ?step label init = let v = Var.create init in of_var (float_slider ~label ?min ?max ?step v) v
  let log ?min ?max label init = let v = Var.create init in of_var (log_slider ~label ?min ?max v) v
  let bool label init = let v = Var.create init in of_var (checkbox ~label v) v
  let choice label options = let v = Var.create (snd (List.hd options)) in of_var (dropdown ~label options v) v
  let buttons label options = let v = Var.create (snd (List.hd options)) in of_var (toggle_buttons ~label options v) v
  let text label init = let v = Var.create init in of_var (text_input ~label v) v
  let color label init = let v = Var.create init in of_var (color_picker ~label v) v
  let range ?min ?max label init = let v = Var.create init in of_var (range_slider ~label ?min ?max v) v
  let var w v = of_var w v
  let const x = { controls = []; value = (fun () -> x) }
  let map f c = { c with value = (fun () -> f (c.value ())) }
  let both a b = { controls = a.controls @ b.controls; value = (fun () -> (a.value (), b.value ())) }
  let ( let+ ) c f = map f c
  let ( and+ ) = both
end

let ( let+ ) = Interact.( let+ )
let ( and+ ) = Interact.( and+ )

let interact ?(manual = false) ?(side = false) (c : t Interact.control) : t =
  let out =
    if not manual then dyn c.value
    else
      (* Widgets are functions, which only [==] can compare. *)
      let result = Var.create ~eq:( == ) (c.value ()) in
      vbox [ button ~kind:"primary" "Run" (fun () -> Var.set result (c.value ())); dyn (fun () -> Var.get result) ]
  in
  let controls = vbox ~gap:"0.35rem" c.controls in
  fun ctx ->
    el ~cls:([ "xui-interact" ] @ if side then [ "xui-side" ] else [])
      "div"
      [ el ~cls:[ "xui-controls" ] "div" [ controls ctx ]; el ~cls:[ "xui-result" ] "div" [ out ctx ] ]

(* -- roots ---------------------------------------------------------------- *)

let css = [%blob "ui.css"]

let show ?id (w : t) =
  let id = match id with Some i -> i | None -> Printf.sprintf "xui-%d" (fresh ()) in
  let rid = fresh () in
  let dead = ref false and error = ref None in
  let rendering = ref false and again = ref false in
  let rec render () =
    if !dead then ()
    else if !rendering then again := true
    else begin
      rendering := true;
      let ctx = { root = id; handlers = Hashtbl.create 16 } in
      let prev = !Var.current in
      Var.current := Some (rid, invalidate);
      let body = try w ctx with e -> el ~cls:[ "xui-error" ] "div" [ txt ("rendering failed: " ^ Printexc.to_string e) ] in
      Var.current := prev;
      let banner =
        match !error with
        | None -> []
        | Some msg ->
            let h = on ctx "xui.dismiss" (fun _ -> error := None; render ()) in
            [ el ~cls:[ "xui-error" ] "div" [ txt msg; el "button" ~cls:[ "xui-button"; "xui-plain" ] ~handlers:[ ("click", h) ] [ txt "dismiss" ] ] ]
      in
      let wrap f v =
        Var.batch (fun () ->
            try f v with e ->
              error := Some ("a handler raised " ^ Printexc.to_string e);
              invalidate ())
      in
      let handlers =
        ("x-ocaml:removed", fun _ -> dead := true)
        :: List.map (fun (k, f) -> (k, wrap f)) (List.of_seq (Hashtbl.to_seq ctx.handlers))
      in
      Widget.display ~id ~handlers (el ~cls:[ "xui" ] "div" ((el "style" [ txt css ]) :: banner @ [ body ]));
      rendering := false;
      if !again then begin
        again := false;
        render ()
      end
    end
  and invalidate () =
    if !Var.depth > 0 then Hashtbl.replace Var.dirty rid render else render ()
  in
  render ()
