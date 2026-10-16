open Parsetree

type call = { caller : string; callee : string; loc : Location.t }

let rec pattern_names p =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> [ txt ]
  | Ppat_constraint (p, _) | Ppat_alias (p, _) -> pattern_names p
  | _ -> []

let tailcall loc =
  Ast_helper.Attr.mk ~loc (Location.mknoloc "tailcall") (PStr [])

(* The reader's own warning attributes could silence warning 51: they go. *)
let is_warning_attr (a : attribute) =
  List.mem a.attr_name.txt [ "warning"; "ocaml.warning"; "warnerror"; "ocaml.warnerror" ]

let no_warnings = List.filter (fun a -> not (is_warning_attr a))

let annotate str =
  let calls = ref [] in
  (* The names of the group being defined, and the function each call is in. *)
  let scope = ref [] and caller = ref "" in
  let open Ast_mapper in
  let rec_bindings m vbs =
    let names = List.concat_map (fun vb -> pattern_names vb.pvb_pat) vbs in
    let saved = !scope and saved_caller = !caller in
    scope := names @ saved;
    let vbs =
      List.map
        (fun vb ->
          caller := String.concat "" (pattern_names vb.pvb_pat);
          m.value_binding m vb)
        vbs
    in
    scope := saved;
    caller := saved_caller;
    vbs
  in
  let expr m e =
    let e = { e with pexp_attributes = no_warnings e.pexp_attributes } in
    match e.pexp_desc with
    | Pexp_let (Recursive, vbs, body) ->
        let vbs = rec_bindings m vbs in
        { e with pexp_desc = Pexp_let (Recursive, vbs, m.expr m body) }
    | Pexp_apply (({ pexp_desc = Pexp_ident { txt = Lident f; _ }; _ } as fn), args)
      when List.mem f !scope ->
        calls := { caller = !caller; callee = f; loc = e.pexp_loc } :: !calls;
        let fn =
          { fn with pexp_attributes = tailcall fn.pexp_loc :: fn.pexp_attributes }
        in
        let args = List.map (fun (l, a) -> (l, m.expr m a)) args in
        { e with pexp_desc = Pexp_apply (fn, args) }
    | _ -> default_mapper.expr m e
  in
  let structure m str =
    List.filter
      (fun si -> match si.pstr_desc with Pstr_attribute a -> not (is_warning_attr a) | _ -> true)
      str
    |> List.map (m.structure_item m)
  in
  let value_binding m vb =
    default_mapper.value_binding m { vb with pvb_attributes = no_warnings vb.pvb_attributes }
  in
  let structure_item m si =
    match si.pstr_desc with
    | Pstr_value (Recursive, vbs) ->
        { si with pstr_desc = Pstr_value (Recursive, rec_bindings m vbs) }
    | _ -> default_mapper.structure_item m si
  in
  let m = { default_mapper with expr; structure; structure_item; value_binding } in
  let str = m.structure m str in
  (str, List.rev !calls)

type result = {
  calls : call list;
  not_tail : call list;
  loaded : Isolated.loaded;
}

let check ?prelude ~name str =
  let str, calls = annotate str in
  let loaded = Isolated.load ~warnings:"+51" ?prelude ~name str in
  let warned = Isolated.warnings 51 loaded.diagnostics in
  let not_tail =
    List.filter
      (fun c -> List.exists (fun (d : Isolated.diagnostic) -> d.loc = c.loc) warned)
      calls
  in
  (* The warnings are the check's, not the reader's mistakes. *)
  let diagnostics =
    List.filter
      (fun (d : Isolated.diagnostic) ->
        match d.kind with Warning { number = 51; _ } -> false | _ -> true)
      loaded.diagnostics
  in
  { calls; not_tail; loaded = { loaded with diagnostics } }
