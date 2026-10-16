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
