open Focs_check_core

let source_and_line name =
  match X_ocaml_cells.source name with
  | Some (line, code) -> (line, code)
  | None -> failwith ("Focs_check: no cell named " ^ name ^ " has run")

let source name = snd (source_and_line name)

type diagnostic = {
  line : int;
  first : int;
  last : int;
  warning : int option;
  error : bool;
  message : string;
}

let diagnostic (d : Isolated.diagnostic) =
  let pos = d.loc.loc_start in
  let none = d.loc = Location.none in
  {
    line = (if none then 0 else pos.pos_lnum);
    first = (if none then 0 else pos.pos_cnum - pos.pos_bol);
    last = (if none then 0 else d.loc.loc_end.pos_cnum - pos.pos_bol);
    warning =
      (match d.kind with Warning { number; _ } -> Some number | Error -> None);
    error =
      (match d.kind with
      | Error | Warning { as_error = true; _ } -> true
      | Warning _ -> false);
    message = d.message;
  }

let string_of_diagnostic d =
  let what =
    match (d.warning, d.error) with
    | None, _ -> "Error"
    | Some n, false -> Printf.sprintf "Warning %d" n
    | Some n, true -> Printf.sprintf "Error (warning %d)" n
  in
  if d.line = 0 then Printf.sprintf "%s: %s" what d.message
  else Printf.sprintf "Line %d, characters %d-%d: %s: %s" d.line d.first d.last what d.message

type loaded = { name : string; ok : bool; diagnostics : diagnostic list }

(* Loading runs a phrase of its own, from within the cell's: the toplevel
   forgets the module it defined when the cell's phrase ends, unless the
   worker reinstates it. *)
let reinstating = ref false

let reinstate_after_phrase () =
  if not !reinstating then begin
    reinstating := true;
    X_ocaml_cells.after_phrase (fun () ->
        reinstating := false;
        Isolated.reinstate ())
  end

let of_loaded (l : Isolated.loaded) =
  { name = l.name; ok = l.ok; diagnostics = List.map diagnostic l.diagnostics }

let load_parsed ?warnings ?prelude ~as_ str =
  reinstate_after_phrase ();
  Isolated.load ?warnings ?prelude ~name:as_ str

let load_code ?warnings ?(line = 1) ~as_ code =
  match Isolated.parse ~line code with
  | Error ds -> { name = as_; ok = false; diagnostics = List.map diagnostic ds }
  | Ok str -> of_loaded (load_parsed ?warnings ~as_ str)

let load ?warnings ~as_ name =
  let line, code = source_and_line name in
  load_code ?warnings ~line ~as_ code

let print_diagnostics l =
  List.iter (fun d -> print_endline (string_of_diagnostic d)) l.diagnostics

(* Tail calls *)

type call = { caller : string; callee : string; line : int }
type tail = { calls : call list; not_tail : call list; tail_loaded : loaded }

let fresh =
  let n = ref 0 in
  fun what ->
    incr n;
    Printf.sprintf "Focs_%s_%d" what !n

let call (c : Tailcheck.call) =
  { caller = c.caller; callee = c.callee; line = c.loc.loc_start.pos_lnum }

let tailcheck_code ?(line = 1) ?as_ code =
  let as_ = match as_ with Some n -> n | None -> fresh "tail" in
  match Isolated.parse ~line code with
  | Error ds ->
      {
        calls = [];
        not_tail = [];
        tail_loaded = { name = as_; ok = false; diagnostics = List.map diagnostic ds };
      }
  | Ok str ->
      reinstate_after_phrase ();
      let r = Tailcheck.check ~name:as_ str in
      {
        calls = List.map call r.calls;
        not_tail = List.map call r.not_tail;
        tail_loaded = of_loaded r.loaded;
      }

let tailcheck ?as_ name =
  let line, code = source_and_line name in
  tailcheck_code ~line ?as_ code

let is_tail_recursive t = t.tail_loaded.ok && t.calls <> [] && t.not_tail = []

let print_tail t =
  if not t.tail_loaded.ok then print_diagnostics t.tail_loaded
  else if t.calls = [] then print_endline "There are no recursive calls."
  else if t.not_tail = [] then print_endline "Every recursive call is a tail call."
  else
    List.iter
      (fun c ->
        Printf.printf "Line %d: this call of %s%s is not a tail call.\n" c.line c.callee
          (if c.caller = c.callee || c.caller = "" then ""
           else Printf.sprintf " (in %s)" c.caller))
      t.not_tail

(* Work *)

let load_counting_code ?(line = 1) ~as_ code =
  match Isolated.parse ~line code with
  | Error ds -> { name = as_; ok = false; diagnostics = List.map diagnostic ds }
  | Ok str ->
      reinstate_after_phrase ();
      of_loaded (Count.load ~name:as_ str)

let load_counting ~as_ name =
  let line, code = source_and_line name in
  load_counting_code ~line ~as_ code

let measure ?fuel ~as_ ~input ~call sizes =
  Count.measure ~name:as_ ?fuel ~input ~call sizes

type growth = { model : string; explanation : string; table : string }

let growth counts =
  let counts =
    List.map
      (function
        | n, Some c -> (n, c)
        | n, None -> failwith (Printf.sprintf "Focs_check.growth: size %d ran out of fuel" n))
      counts
  in
  if List.length counts < 3 then failwith "Focs_check.growth: fewer than three sizes";
  let v = Growth.classify counts in
  { model = Growth.name v.best; explanation = Growth.explain v; table = Growth.table counts }

let print_growth g =
  print_string g.table;
  print_endline g.explanation

(* What it uses *)

type construct = While | For | Let_rec | Recursion | Mutation | Try | Array

type rule =
  | Forbid_value of string
  | Forbid_module of string
  | Forbid of construct
  | Require of construct
  | Require_value of string
  | Forbid_library of string list

let construct : construct -> Ban.construct = function
  | While -> While
  | For -> For
  | Let_rec -> Let_rec
  | Recursion -> Recursion
  | Mutation -> Mutation
  | Try -> Try
  | Array -> Array

let rule : rule -> Ban.rule = function
  | Forbid_value v -> Forbid_value v
  | Forbid_module m -> Forbid_module m
  | Forbid c -> Forbid (construct c)
  | Require c -> Require (construct c)
  | Require_value v -> Require_value v
  | Forbid_library allowed -> Forbid_library allowed

let typed ?(line = 1) code =
  let fail ds =
    failwith
      (String.concat "\n" (List.map (fun d -> string_of_diagnostic (diagnostic d)) ds))
  in
  match Isolated.parse ~line code with
  | Error ds -> fail ds
  | Ok str -> (
      match Isolated.typecheck ~warnings:"-a" str with
      | Ok t, _ -> t
      | Error (), ds -> fail ds)

let check_uses_code ?line rules code =
  Ban.check (List.map rule rules) (typed ?line code)
  |> List.map (fun (v : Ban.violation) ->
         ((if v.loc = Location.none then 0 else v.loc.loc_start.pos_lnum), v.message))

let check_uses rules name =
  let line, code = source_and_line name in
  check_uses_code ~line rules code

let uses name =
  let line, code = source_and_line name in
  List.sort_uniq compare (List.map fst (Ban.uses (typed ~line code)))

let recursive_functions name =
  let line, code = source_and_line name in
  Ban.recursive_functions (typed ~line code)
