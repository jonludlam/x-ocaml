open Code_mirror
open State
open View

type t = {
  view : EditorView.t;
  lines_comp : Compartment.t;
  merlin_comp : Compartment.t;
  mutable merlin_extension : unit -> Extension.t list;
  changes : Compartment.t;
  mutable previous_lines : int;
  mutable current_doc : string;
  mutable messages : (int * Brr.El.t list) list;
  folds : (int * int) list ref;  (* line ranges the page asks to fold *)
}

let find_line_ends at doc =
  let rec go i =
    if i >= String.length doc || doc.[i] = '\n' then i else go (i + 1)
  in
  go at

(* Message positions come from the toplevel as byte offsets into the UTF-8
   source; CodeMirror counts UTF-16 code units. *)
let utf16_offset doc at =
  let rec go i n =
    if i >= at then n
    else
      let d = String.get_utf_8_uchar doc i in
      go
        (i + Uchar.utf_decode_length d)
        (n + (Uchar.utf_16_byte_length (Uchar.utf_decode_uchar d) / 2))
  in
  go 0 0

(* The messages are block widgets in a state field. The field maps them
   through every change, so they never point past the document, and it is
   replaced wholesale by this effect when the messages change. *)
let decorations = RangeSet.conv_of Decoration.conv
let set_messages_effect = StateEffectType.define decorations

let messages_field =
  StateField.define decorations
    ~create:(fun _ -> Decoration.none)
    ~update:(fun decos tr ->
      List.fold_left
        (fun decos e ->
          Option.value ~default:decos (StateEffect.value e set_messages_effect))
        (RangeSet.map decos (ChangeSet.desc (Transaction.changes tr)))
        (Transaction.effects tr))
    ~provide:(fun field -> Facet.from EditorView.decorations field)

(* Lines the page marks, as line decorations: the same kind of state field. *)
let set_marked_effect = StateEffectType.define decorations

let marked_field =
  StateField.define decorations
    ~create:(fun _ -> Decoration.none)
    ~update:(fun decos tr ->
      List.fold_left
        (fun decos e ->
          Option.value ~default:decos (StateEffect.value e set_marked_effect))
        (RangeSet.map decos (ChangeSet.desc (Transaction.changes tr)))
        (Transaction.effects tr))
    ~provide:(fun field -> Facet.from EditorView.decorations field)

(* The page's fold ranges, offered wherever their first line is: the code
   is OCaml in a legacy mode, with no syntax tree for CodeMirror to fold. *)
let fold_ranges folds =
  Facet.of_ Language.fold_service (fun state ~line_start ~line_end ->
      let doc = EditorState.doc state in
      let first = Line.number (Text.line_at doc line_start) in
      match List.assoc_opt first !folds with
      | Some last when last > first && last <= Text.lines doc ->
          Some (line_end, Line.to_ (Text.line doc last))
      | _ -> None)

let render_messages cm =
  let doc = cm.current_doc in
  List.map (fun (at, msg) ->
      Decoration.range ~to_:at ~from:at
      @@ Decoration.widget ~block:true ~side:99
      @@ WidgetType.make ~to_dom:(fun _ -> msg) ())
  @@ List.map (fun (at, msg) ->
      (utf16_offset doc (find_line_ends (min at (String.length doc)) doc), msg))
  @@ List.concat
  @@ List.map (fun (loc, lst) -> List.map (fun m -> (loc, m)) lst)
  @@ List.sort (fun (a, _) (b, _) -> Int.compare a b) cm.messages
  |> RangeSet.of_ Decoration.conv

let dispatch_effects ed effects =
  EditorView.dispatch ed.view (TransactionSpec.create ~effects ())

let reconfigure ed comp ext =
  dispatch_effects ed [ Compartment.reconfigure comp ext ]

let refresh_messages ed =
  dispatch_effects ed
    [ StateEffectType.of_ set_messages_effect (render_messages ed) ]

let custom_ln editor =
  line_numbers
    ~format_number:(fun x _ -> string_of_int (editor.previous_lines + x))
    ()

let refresh_lines ed = reconfigure ed ed.lines_comp (custom_ln ed)

let refresh_merlin ed =
  reconfigure ed ed.merlin_comp (Extension.of_list (ed.merlin_extension ()))

let configure_merlin ed extension =
  ed.merlin_extension <- extension;
  refresh_merlin ed

let clear x =
  x.messages <- [];
  refresh_lines x;
  refresh_messages x;
  refresh_merlin x

let source_of_state s = Text.to_string (EditorState.doc s)
let source t = source_of_state @@ EditorView.state t.view

let prefix_length a b =
  let rec go i =
    if i >= String.length a || i >= String.length b || a.[i] <> b.[i] then i
    else go (i + 1)
  in
  go 0

(* Syntax colours as classes rather than CodeMirror's default style, whose
   generated class names a stylesheet cannot name, so that style.css can
   take them from --xo-syntax-* variables. *)
let highlighting =
  let open Language in
  let cls c tags = TagStyle.make ~class_:("xo-" ^ c) tags in
  syntax_highlighting
    (HighlightStyle.define
       Tags.
         [
           cls "keyword" [ keyword ];
           cls "atom" [ atom; bool; label_name ];
           cls "literal" [ literal ];
           cls "string" [ string ];
           cls "escape" [ escape; special string ];
           cls "definition" [ definition variable_name ];
           cls "local" [ local variable_name ];
           cls "type" [ type_name; namespace ];
           cls "class" [ class_name ];
           cls "special" [ special variable_name; macro_name ];
           cls "property" [ definition property_name ];
           cls "comment" [ comment ];
           cls "meta" [ meta ];
           cls "invalid" [ invalid ];
         ])

let make ?(read_only = false) parent =
  let changes = Compartment.make () in
  let lines = Compartment.make () in
  let merlin = Compartment.make () in
  let empty = Extension.of_list [] in
  let folds = ref [] in
  let extensions =
    Extension.of_list
      [
        basic_setup;
        EditorView.line_wrapping_extension;
        Compartment.of_ lines empty;
        StateField.extension messages_field;
        StateField.extension marked_field;
        fold_ranges folds;
        Compartment.of_ changes empty;
        Compartment.of_ merlin empty;
        Facet.of_ EditorState.read_only_facet read_only;
        highlighting;
      ]
  in
  let config = EditorStateConfig.create ~doc:"" ~extensions () in
  let state = EditorState.create ~config () in
  let view =
    EditorView.create ~config:(EditorViewConfig.create ~state ~parent ()) ()
  in
  {
    previous_lines = 0;
    current_doc = "";
    messages = [];
    view;
    lines_comp = lines;
    merlin_comp = merlin;
    merlin_extension = (fun () -> []);
    changes;
    folds;
  }

let set_current_doc t new_doc =
  let at = prefix_length t.current_doc new_doc in
  t.current_doc <- new_doc;
  t.messages <- List.filter (fun (loc, _) -> loc < at) t.messages;
  refresh_messages t

let on_change cm fn =
  let has_changed =
    Facet.of_ EditorView.update_listener (fun ev ->
        if ViewUpdate.doc_changed ev then
          let new_doc = source_of_state (ViewUpdate.state ev) in
          if not (String.equal cm.current_doc new_doc) then (
            set_current_doc cm new_doc;
            fn ()))
  in
  reconfigure cm cm.changes has_changed

let count_lines str =
  if str = "" then 0
  else
    let nb = ref 1 in
    for i = 0 to String.length str - 1 do
      if str.[i] = '\n' then incr nb
    done;
    !nb

let nb_lines t = t.previous_lines + count_lines t.current_doc
let get_previous_lines t = t.previous_lines

let set_previous_lines t nb =
  t.previous_lines <- nb;
  refresh_lines t

let set_messages t msg =
  t.messages <- msg;
  refresh_messages t

let clear_messages t = set_messages t []
let add_message t loc msg = set_messages t ((loc, msg) :: t.messages)

(* CodeMirror stores lines joined by "\n" whatever the source used. *)
let normalise_newlines doc =
  String.concat "\n"
  @@ List.map (fun l ->
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
  @@ String.split_on_char '\n' doc

(* Replace the document and drop the messages in one transaction, so no
   message is ever mapped through a change that removes the text it follows.
   [current_doc] is updated first, as CodeMirror will store it, so the change
   listener does not take this for an edit. *)
let set_source t doc =
  let doc = normalise_newlines doc in
  t.messages <- [];
  t.current_doc <- doc;
  let len = Text.length (EditorState.doc (EditorView.state t.view)) in
  EditorView.dispatch t.view
    (TransactionSpec.create
       ~changes:(ChangeSpec.replace ~from:0 ~to_:len ~insert:doc ())
       ~effects:[ StateEffectType.of_ set_messages_effect Decoration.none ]
       ())

(* [set_layout t ~fold ~mark]: fold each range of lines [(first, last)]
   behind its first line, and mark each line in [mark]'s ranges, as the
   page's [fold] and [highlight] attributes ask. Lines count from 1, in this
   cell; ranges that don't fit the source are left out. *)
let set_layout t ~fold ~mark =
  let doc = EditorState.doc (EditorView.state t.view) in
  let n = Text.lines doc in
  let fits (a, b) = 1 <= a && a <= b && b <= n in
  let fold = List.filter (fun (a, b) -> fits (a, b) && a < b) fold in
  t.folds := fold;
  let fold_effects =
    List.map (fun (a, b) ->
        StateEffectType.of_ Language.fold_effect
          (Line.to_ (Text.line doc a), Line.to_ (Text.line doc b)))
      fold
  in
  let marked =
    List.concat_map
      (fun (a, b) -> if fits (a, b) then List.init (b - a + 1) (fun i -> a + i) else [])
      mark
    |> List.sort_uniq Int.compare
    |> List.map (fun l ->
           Decoration.range ~from:(Line.from (Text.line doc l))
             (Decoration.line ~class_:"xo-marked" ()))
  in
  dispatch_effects t
    (StateEffectType.of_ set_marked_effect (RangeSet.of_ Decoration.conv marked)
    :: fold_effects)
