open Code_mirror
open State
open View

type t = {
  view : EditorView.t;
  messages_comp : Compartment.t;
  lines_comp : Compartment.t;
  merlin_comp : Compartment.t;
  mutable merlin_extension : unit -> Extension.t list;
  changes : Compartment.t;
  mutable previous_lines : int;
  mutable current_doc : string;
  mutable messages : (int * Brr.El.t list) list;
}

let find_line_ends at doc =
  let rec go i =
    if i >= String.length doc || doc.[i] = '\n' then i else go (i + 1)
  in
  go at

let render_messages cm =
  let doc = cm.current_doc in
  let ranges =
    List.map (fun (at, msg) ->
        Decoration.range ~to_:at ~from:at
        @@ Decoration.widget ~block:true ~side:99
        @@ WidgetType.make ~to_dom:(fun _ -> msg) ())
    @@ List.filter (fun (at, _) -> at <= String.length doc)
    @@ List.map (fun (at, msg) ->
        let at = find_line_ends at doc in
        (at, msg))
    @@ List.concat
    @@ List.map (fun (loc, lst) -> List.map (fun m -> (loc, m)) lst)
    @@ List.sort (fun (a, _) (b, _) -> Int.compare a b) cm.messages
  in
  Facet.of_ EditorView.decorations (RangeSet.of_ Decoration.conv ranges)

let reconfigure ed comp ext =
  EditorView.dispatch ed.view
    (TransactionSpec.create ~effects:[ Compartment.reconfigure comp ext ] ())

let refresh_messages ed = reconfigure ed ed.messages_comp (render_messages ed)

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

let make parent =
  let changes = Compartment.make () in
  let messages = Compartment.make () in
  let lines = Compartment.make () in
  let merlin = Compartment.make () in
  let empty = Extension.of_list [] in
  let extensions =
    Extension.of_list
      [
        basic_setup;
        EditorView.line_wrapping_extension;
        Compartment.of_ lines empty;
        Compartment.of_ messages empty;
        Compartment.of_ changes empty;
        Compartment.of_ merlin empty;
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
    messages_comp = messages;
    lines_comp = lines;
    merlin_comp = merlin;
    merlin_extension = (fun () -> []);
    changes;
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

let set_source t doc =
  set_current_doc t doc;
  EditorView.set_doc t.view doc
