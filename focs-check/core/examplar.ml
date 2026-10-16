open Parsetree

type impl = { name : string; about : string; code : string }

type example = {
  source : string;
  line : int;
  valid : bool;
  wheats_failing : string list;
  catches : string list;
  diagnostics : Isolated.diagnostic list;
}

type result = { examples : example list; caught : impl list; missed : impl list }

let source_of code (loc : Location.t) =
  let a = loc.loc_start.pos_cnum and b = loc.loc_end.pos_cnum in
  if a >= 0 && b <= String.length code && a <= b then String.sub code a (b - a) else ""

(* An example as a phrase that raises if it does not hold. *)
let as_check si =
  let open Ast_helper in
  let loc = si.pstr_loc in
  match si.pstr_desc with
  | Pstr_eval (({ pexp_desc = Pexp_assert _; _ } as e), _) ->
      Str.value ~loc Nonrecursive [ Vb.mk ~loc (Pat.construct ~loc (Location.mknoloc (Longident.Lident "()")) None) e ]
  | Pstr_eval (e, _) ->
      let fail = Exp.assert_ ~loc (Exp.construct ~loc (Location.mknoloc (Longident.Lident "false")) None) in
      let body =
        Exp.ifthenelse ~loc
          (Exp.apply ~loc (Exp.ident ~loc (Location.mknoloc (Longident.Lident "not"))) [ (Nolabel, e) ])
          fail None
      in
      Str.value ~loc Nonrecursive [ Vb.mk ~loc (Pat.construct ~loc (Location.mknoloc (Longident.Lident "()")) None) body ]
  | _ -> si

let counter = ref 0

let passes impl item =
  incr counter;
  let name = Printf.sprintf "Focs_example_%d" !counter in
  let prelude = Isolated.prelude impl.code in
  let l = Isolated.load ~warnings:"-a" ~prelude ~name [ item ] in
  (l.ok, l.diagnostics)

(* One example a line: a line that starts at its first column starts an
   example, and indented lines go on with it, so that examples need no ;;
   between them. Each comes with the line it starts at. *)
let chunks ~line code =
  let lines = String.split_on_char '\n' code in
  let starts l = l <> "" && l.[0] <> ' ' && l.[0] <> '\t' in
  let rec go acc current n = function
    | [] -> List.rev (match current with Some c -> c :: acc | None -> acc)
    | l :: rest ->
        if starts l then
          go (match current with Some c -> c :: acc | None -> acc) (Some (n, [ l ])) (n + 1) rest
        else
          match current with
          | Some (start, ls) -> go acc (Some (start, l :: ls)) (n + 1) rest
          | None -> go acc None (n + 1) rest
  in
  go [] None line lines
  |> List.map (fun (start, ls) -> (start, String.concat "\n" (List.rev ls)))

let run_item ~wheats ~chaffs code si =
  let item = as_check si in
  let results = List.map (fun w -> (w, passes w item)) wheats in
  let diagnostics =
    match results with (_, (_, ds)) :: _ -> Isolated.errors ds | [] -> []
  in
  (* A compile error is not an exception: the example is broken. *)
  let compiles =
    List.for_all (fun (d : Isolated.diagnostic) -> d.loc = Location.none) diagnostics
  in
  let wheats_failing =
    List.filter_map (fun (w, (ok, _)) -> if ok then None else Some w.name) results
  in
  let valid = compiles && wheats_failing = [] in
  let catches =
    if not valid then []
    else List.filter_map (fun c -> if fst (passes c item) then None else Some c.name) chaffs
  in
  {
    source = String.trim (source_of code si.pstr_loc);
    line = si.pstr_loc.loc_start.pos_lnum;
    valid;
    wheats_failing;
    catches;
    diagnostics = (if compiles then [] else diagnostics);
  }

(* [let examples = [ e1; e2 ]] is examples e1 and e2. *)
let rec list_items (e : expression) =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ (_, x); (_, rest) ]; _ }) ->
      Option.map (fun xs -> x :: xs) (list_items rest)
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> Some []
  | _ -> None

let items str =
  List.concat_map
    (fun si ->
      match si.pstr_desc with
      | Pstr_value (_, [ vb ]) -> (
          match list_items vb.pvb_expr with
          | Some (_ :: _ as es) ->
              List.map (fun e -> Ast_helper.Str.eval ~loc:e.pexp_loc e) es
          | _ -> [ si ])
      | _ -> [ si ])
    str

let run ~wheats ~chaffs ?(line = 1) code =
  let examples =
    match Isolated.parse ~line code with
    | Ok str -> List.map (run_item ~wheats ~chaffs code) (items str)
    | Error _ ->
        List.concat_map
          (fun (start, chunk) ->
            match Isolated.parse ~line:start chunk with
            | Error ds ->
                [ { source = String.trim chunk; line = start; valid = false;
                    wheats_failing = []; catches = []; diagnostics = ds } ]
            | Ok str -> List.map (run_item ~wheats ~chaffs chunk) (items str))
          (chunks ~line code)
  in
  let caught, missed =
    List.partition (fun c -> List.exists (fun e -> List.mem c.name e.catches) examples) chaffs
  in
  { examples; caught; missed }
