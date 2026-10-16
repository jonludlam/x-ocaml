open Typedtree

type construct = While | For | Let_rec | Recursion | Mutation | Try | Array

type rule =
  | Forbid_value of string
  | Forbid_module of string
  | Forbid of construct
  | Require of construct
  | Require_value of string
  | Forbid_library of string list

type violation = { rule : rule; loc : Location.t; message : string }

let construct_name = function
  | While -> "while"
  | For -> "for"
  | Let_rec -> "let rec"
  | Recursion -> "recursion"
  | Mutation -> "mutation"
  | Try -> "exception handling"
  | Array -> "arrays"

(* What the code holds, gathered in one pass. *)
type found = {
  mutable uses : (string * Location.t) list;
  mutable constructs : (construct * Location.t) list;
  mutable recursive : string list;
  mutable stdlib_written : Location.t list;
      (* uses written Stdlib.x, which a ban on the library catches even for
         what is open by default, as shadowing Stdlib would *)
}

(* The standard library's modules are aliases of Stdlib__List and the like,
   which is where normalising leads: name them as written. *)
let canonical name =
  let prefix = "Stdlib__" in
  let n = String.length prefix in
  if String.length name > n && String.sub name 0 n = prefix then
    "Stdlib." ^ String.sub name n (String.length name - n)
  else name

let path_name env loc path =
  let path = try Env.normalize_value_path (Some loc) env path with _ -> path in
  canonical (Path.name path)

let rec idents_of_pattern : type k. k general_pattern -> Ident.t list =
 fun p ->
  match p.pat_desc with
  | Tpat_var (id, _, _) -> [ id ]
  | Tpat_alias (p, id, _, _, _) -> id :: idents_of_pattern p
  | _ -> []

let gather str =
  let found = { uses = []; constructs = []; recursive = []; stdlib_written = [] } in
  let add c loc = found.constructs <- (c, loc) :: found.constructs in
  (* The groups of let rec being defined, innermost first. *)
  let groups = ref [] in
  let open Tast_iterator in
  let rec_group it vbs =
    let ids = List.concat_map (fun vb -> idents_of_pattern vb.vb_pat) vbs in
    let called = ref false in
    groups := (ids, called) :: !groups;
    List.iter (it.value_binding it) vbs;
    groups := List.tl !groups;
    if !called then
      found.recursive <- List.map Ident.name ids @ found.recursive;
    List.iter (fun vb -> add Let_rec vb.vb_loc) vbs;
    if !called then List.iter (fun vb -> add Recursion vb.vb_loc) vbs
  in
  let expr it e =
    (match e.exp_desc with
    | Texp_ident (_, { txt = lid; _ }, _)
      when List.hd (Longident.flatten lid) = "Stdlib" ->
        found.stdlib_written <- e.exp_loc :: found.stdlib_written
    | _ -> ());
    (match e.exp_desc with
    | Texp_ident ((Path.Pident id as path), _, _) -> (
        match List.find_opt (fun (ids, _) -> List.exists (Ident.same id) ids) !groups with
        | Some (_, called) -> called := true
        | None ->
            (* Local, unless it is global: the toplevel's, or a module's. *)
            if Ident.global id then
              found.uses <- (path_name e.exp_env e.exp_loc path, e.exp_loc) :: found.uses)
    | Texp_ident (path, _, _) ->
        found.uses <- (path_name e.exp_env e.exp_loc path, e.exp_loc) :: found.uses
    | Texp_while _ -> add While e.exp_loc
    | Texp_for _ -> add For e.exp_loc
    | Texp_setfield _ -> add Mutation e.exp_loc
    | Texp_array (_, _ :: _) -> add Array e.exp_loc
    | Texp_try _ -> add Try e.exp_loc
    | Texp_match (_, _, _ :: _, _) -> add Try e.exp_loc
    | _ -> ());
    match e.exp_desc with
    | Texp_let (Recursive, vbs, body) ->
        rec_group it vbs;
        it.expr it body
    | _ -> default_iterator.expr it e
  in
  let structure_item it si =
    match si.str_desc with
    | Tstr_value (Recursive, vbs) -> rec_group it vbs
    | _ -> default_iterator.structure_item it si
  in
  let it = { default_iterator with expr; structure_item } in
  it.structure it str;
  (* Mutation through the standard library counts as mutation. *)
  List.iter
    (fun (u, loc) ->
      if List.mem u [ "Stdlib.:="; "Stdlib.ref"; "Stdlib.Array.set"; "Stdlib.Array.make" ]
      then add Mutation loc)
    found.uses;
  found.uses <- List.rev found.uses;
  found

let uses str = (gather str).uses
let recursive_functions str = List.rev (gather str).recursive

(* The module a value comes from, below Stdlib: Some "Stdlib.List" for
   Stdlib.List.rev, None for Stdlib.( + ), which is open by default. *)
let library_module u =
  match String.split_on_char '.' u with
  | "Stdlib" :: (_ :: _ :: _ as rest) ->
      let rec drop_last = function [ _ ] | [] -> [] | x :: xs -> x :: drop_last xs in
      Some (String.concat "." ("Stdlib" :: drop_last rest))
  | _ -> None

let in_module m u =
  String.length u > String.length m
  && String.sub u 0 (String.length m + 1) = m ^ "."

let check rules str =
  let found = gather str in
  let whole = Location.none in
  List.concat_map
    (fun rule ->
      match rule with
      | Forbid_value v ->
          List.filter_map
            (fun (u, loc) ->
              if u = v then Some { rule; loc; message = v ^ " is not allowed here" }
              else None)
            found.uses
      | Forbid_module m ->
          List.filter_map
            (fun (u, loc) ->
              if in_module m u then
                Some { rule; loc; message = Printf.sprintf "%s (from %s) is not allowed here" u m }
              else None)
            found.uses
      | Forbid c ->
          List.filter_map
            (fun (c', loc) ->
              if c = c' then
                Some { rule; loc; message = construct_name c ^ " is not allowed here" }
              else None)
            found.constructs
      | Require c ->
          if List.exists (fun (c', _) -> c = c') found.constructs then []
          else [ { rule; loc = whole; message = "this must use " ^ construct_name c } ]
      | Forbid_library allowed ->
          let qualified =
            List.filter_map
              (fun (u, loc) ->
                match library_module u with
                | Some m when not (List.exists (fun a -> m = a || in_module a m) allowed) ->
                    Some { rule; loc; message = Printf.sprintf "%s is a library function: write it yourself" u }
                | _ -> None)
              found.uses
          in
          let written =
            List.filter_map
              (fun loc ->
                if List.exists (fun (v : violation) -> v.loc = loc) qualified then None
                else Some { rule; loc; message = "the library (Stdlib) is not allowed here" })
              (List.rev found.stdlib_written)
          in
          qualified @ written
      | Require_value v ->
          if List.exists (fun (u, _) -> u = v) found.uses then []
          else [ { rule; loc = whole; message = "this must use " ^ v } ])
    rules
