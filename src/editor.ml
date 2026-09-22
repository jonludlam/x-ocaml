open Code_mirror

type t = {
  view : View.EditorView.t;
  messages_comp : State.Compartment.t;
  lines_comp : State.Compartment.t;
  merlin_comp : State.Compartment.t;
  mutable merlin_extension : unit -> Extension.t list;
  changes : State.Compartment.t;
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
        View.Decoration.range ~from:at ~to_:at
        @@ View.Decoration.widget ~block:true ~side:99
        @@ View.WidgetType.make (fun () -> msg))
    @@ List.filter (fun (at, _) -> at <= String.length doc)
    @@ List.map (fun (at, msg) ->
           let at = find_line_ends at doc in
           (at, msg))
    @@ List.concat
    @@ List.map (fun (loc, lst) -> List.map (fun m -> (loc, m)) lst)
    @@ List.sort (fun (a, _) (b, _) -> Int.compare a b) cm.messages
  in
  let set =
    match State.RangeSet.of_ ranges with
    | Some set -> set
    | None -> View.Decoration.none
  in
  State.Facet.of_ View.EditorView.decorations set

let reconfigure ed comp extensions =
  View.EditorView.dispatch ed.view
    (State.TransactionSpec.create
       ~effects:[ State.Compartment.reconfigure comp extensions ]
       ())

let refresh_messages ed =
  reconfigure ed ed.messages_comp [ render_messages ed ]

let custom_ln editor =
  View.line_numbers
    ~format:(fun x -> string_of_int (editor.previous_lines + x))
    ()

let refresh_lines ed = reconfigure ed ed.lines_comp [ custom_ln ed ]
let refresh_merlin ed = reconfigure ed ed.merlin_comp (ed.merlin_extension ())

let configure_merlin ed extension =
  ed.merlin_extension <- extension;
  refresh_merlin ed

let clear x =
  x.messages <- [];
  refresh_lines x;
  refresh_messages x;
  refresh_merlin x

let source_of_state s =
  String.concat "\n" @@ Array.to_list @@ Array.map Jstr.to_string
  @@ State.Text.to_jstr_array @@ State.EditorState.doc s

let source t = source_of_state @@ View.EditorView.state t.view

let prefix_length a b =
  let rec go i =
    if i >= String.length a || i >= String.length b || a.[i] <> b.[i] then i
    else go (i + 1)
  in
  go 0

let basic_setup = Jv.get Jv.global "__CM__basic_setup" |> Extension.of_jv

let make parent =
  let changes = State.Compartment.make () in
  let messages = State.Compartment.make () in
  let lines = State.Compartment.make () in
  let merlin = State.Compartment.make () in
  let extensions =
    [
      basic_setup;
      View.EditorView.line_wrapping ();
      State.Compartment.of_ lines [];
      State.Compartment.of_ messages [];
      State.Compartment.of_ changes [];
      State.Compartment.of_ merlin [];
    ]
  in
  let config = State.EditorStateConfig.create ~doc:"" ~extensions () in
  let state = State.EditorState.create ~config () in
  let view =
    View.EditorView.create
      ~config:(View.EditorViewConfig.create ~state ~parent ())
      ()
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
    State.Facet.of_ View.EditorView.update_listener (fun ev ->
        if View.EditorView.Update.doc_changed ev then
          let new_doc = source_of_state (View.EditorView.Update.state ev) in
          if not (String.equal cm.current_doc new_doc) then (
            set_current_doc cm new_doc;
            fn ()))
  in
  reconfigure cm cm.changes [ has_changed ]

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
  let state = View.EditorView.state t.view in
  let changes =
    {
      State.TransactionSpec.from = 0;
      to_ = Some (State.Text.length (State.EditorState.doc state));
      insert = Some doc;
    }
  in
  View.EditorView.dispatch t.view (State.TransactionSpec.create ~changes ())
