type kind = Error | Warning of { number : int; as_error : bool }
type diagnostic = { kind : kind; loc : Location.t; message : string }

let pp_diagnostic ppf d =
  let pos = d.loc.loc_start and fin = d.loc.loc_end in
  let what =
    match d.kind with
    | Error -> "Error"
    | Warning { number; as_error = false } -> Printf.sprintf "Warning %d" number
    | Warning { number; as_error = true } ->
        Printf.sprintf "Error (warning %d)" number
  in
  if d.loc = Location.none then Format.fprintf ppf "%s: %s" what d.message
  else
    Format.fprintf ppf "Line %d, characters %d-%d: %s: %s" pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      (fin.pos_cnum - pos.pos_bol)
      what d.message

let warnings n =
  List.filter (fun d ->
      match d.kind with Warning { number; _ } -> number = n | Error -> false)


let errors =
  List.filter (fun d ->
      match d.kind with Error | Warning { as_error = true; _ } -> true | _ -> false)

let doc_to_string doc = Format.asprintf "%a" Format_doc.Doc.format doc

(* The number of a warning's id, as "51 [wrong-tailcall-expectation]". *)
let number_of_id id =
  let digits = String.to_seq id |> Seq.take_while (fun c -> c >= '0' && c <= '9') in
  Option.value ~default:0 (int_of_string_opt (String.of_seq digits))

let prelude_fname = "_prelude_"
let in_prelude d = d.loc.loc_start.pos_fname = prelude_fname

(* What the compiler reports while [f] runs, kept rather than printed. *)
let capturing ?warnings f =
  let diags = ref [] in
  let add d = diags := d :: !diags in
  let state = Warnings.backup () in
  let reporter = !Location.warning_reporter in
  Location.warning_reporter :=
    (fun loc w ->
      (match Warnings.report w with
      | `Active { id; message; is_error; _ } ->
          add
            {
              kind = Warning { number = number_of_id id; as_error = is_error };
              loc;
              message = doc_to_string message;
            }
      | `Inactive -> ());
      None);
  Option.iter (fun w -> ignore (Warnings.parse_options false w)) warnings;
  let result =
    Fun.protect
      ~finally:(fun () ->
        (* Warnings made errors are counted, and the toplevel's next phrase
           would fail for them: they were ours. *)
        Warnings.reset_fatal ();
        Warnings.restore state;
        Location.warning_reporter := reporter)
      (fun () -> f add)
  in
  (result, List.rev !diags)

let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok (report : Location.report)) ->
      Some
        {
          kind = Error;
          loc = report.main.loc;
          message = doc_to_string report.main.txt;
        }
  | Some `Already_displayed -> None
  | None -> (
      match exn with
      | Warnings.Errors -> None (* the warnings made errors are kept *)
      | _ ->
          Some
            {
              kind = Error;
              loc = Location.none;
              message = Printexc.to_string exn;
            })

let parse ?(fname = "") ?(line = 1) code =
  let lexbuf = Lexing.from_string code in
  Lexing.set_position lexbuf
    { pos_fname = fname; pos_lnum = line; pos_bol = 0; pos_cnum = 0 };
  Location.input_name := fname;
  match Parse.implementation lexbuf with
  | str -> Ok str
  | exception exn -> Error (Option.to_list (error_of_exn exn))

let typecheck ?warnings ?env str =
  let env = Option.value env ~default:!Toploop.toplevel_env in
  capturing ?warnings (fun add ->
      match
        Typecore.reset_delayed_checks ();
        let tstr, _, _, _, _ = Typemod.type_structure env str in
        Typecore.force_delayed_checks ();
        tstr
      with
      | tstr -> Ok tstr
      | exception exn ->
          Option.iter add (error_of_exn exn);
          Error ())

type loaded = {
  name : string;
  ok : bool;
  diagnostics : diagnostic list;
  exn : exn option;
}

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) ignore

(* The modules loaded since the worker last reinstated them. *)
let loaded_modules : (Ident.t * Types.module_declaration) list ref = ref []

exception Raised of string

let execute ppf phrase =
  Toploop.execute_phrase false ppf (Parsetree.Ptop_def phrase)

let load ?warnings ?(prelude = []) ~name str =
  let open Ast_helper in
  let mb =
    Mb.mk (Location.mknoloc (Some name)) (Mod.structure (prelude @ str))
  in
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  let ok, diagnostics =
    capturing ?warnings (fun add ->
        match execute ppf [ Str.module_ mb ] with
        | true -> true
        | false ->
            (* It raised: the toplevel says what, and goes on. *)
            Format.pp_print_flush ppf ();
            add
              {
                kind = Error;
                loc = Location.none;
                message = String.trim (Buffer.contents buf);
              };
            false
        | exception exn ->
            Option.iter add (error_of_exn exn);
            false)
  in
  let diagnostics = List.filter (fun d -> not (in_prelude d)) diagnostics in
  if ok then begin
    match Env.find_module_by_name (Longident.Lident name) !Toploop.toplevel_env with
    | Path.Pident id, md -> loaded_modules := !loaded_modules @ [ (id, md) ]
    | _ | (exception Not_found) -> ()
  end;
  let exn =
    if ok then None
    else
      List.find_map
        (fun d ->
          if d.kind = Error && d.loc = Location.none then Some (Raised d.message)
          else None)
        diagnostics
  in
  { name; ok; diagnostics; exn }

let counter = ref 0

let value ?loc ~typ e =
  incr counter;
  let name = Printf.sprintf "focs_check_value__%d" !counter in
  let fail diags =
    failwith
      (String.concat "\n"
         (List.map (Format.asprintf "%a" pp_diagnostic) diags))
  in
  let code = Printf.sprintf "let %s : (%s) = (%s)" name typ e in
  let line = Option.fold ~none:1 ~some:(fun (l : Location.t) -> l.loc_start.pos_lnum) loc in
  match parse ~line code with
  | Error diags -> fail diags
  | Ok str -> (
      let ok, diags =
        capturing ~warnings:"-a" (fun add ->
            match execute null_formatter str with
            | b -> b
            | exception exn ->
                Option.iter add (error_of_exn exn);
                false)
      in
      if not ok then fail diags;
      match Env.find_value_by_name (Longident.Lident name) !Toploop.toplevel_env with
      | Path.Pident id, _ -> Toploop.getvalue (Translmod.toplevel_name id)
      | _ | (exception Not_found) -> failwith ("focs-check: lost " ^ name))

let reinstate () =
  List.iter
    (fun (id, md) ->
      Toploop.toplevel_env :=
        Env.add_module_declaration ~check:false id Mp_present md
          !Toploop.toplevel_env)
    !loaded_modules;
  loaded_modules := []

let prelude code =
  match parse ~fname:prelude_fname code with
  | Ok str -> str
  | Error diags ->
      failwith
        (String.concat "\n" (List.map (Format.asprintf "%a" pp_diagnostic) diags))
