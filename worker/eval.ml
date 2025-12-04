open Js_of_ocaml_toplevel
open X_protocol

module Value_env : sig
  type t

  val empty : t
  val capture : t -> Ident.t list -> t
  val restore : t -> unit
end = struct
  module String_map = Map.Make (String)

  type t = Obj.t String_map.t

  let empty = String_map.empty

  let capture t idents =
    List.fold_left
      (fun t ident ->
        let name = Translmod.toplevel_name ident in
        let v = Toploop.getvalue name in
        String_map.add name v t)
      t idents

  let restore t = String_map.iter (fun name v -> Toploop.setvalue name v) t
end

module Environment = struct
  (* Track environments by filename. Each filename has its own environment.
     When filename is None, we use the default sequential behavior with cell IDs. *)
  module Filename_map = Map.Make(String)

  type env_state = Env.t * Value_env.t

  let file_environments : env_state Filename_map.t ref = ref Filename_map.empty
  let default_env : env_state ref = ref (!Toploop.toplevel_env, Value_env.empty)
  let environments = ref []

  let init () =
    default_env := (!Toploop.toplevel_env, Value_env.empty);
    environments := [ (0, !Toploop.toplevel_env, Value_env.empty) ]

  let reset_by_filename filename =
    let (typing_env, value_env) =
      try Filename_map.find filename !file_environments
      with Not_found -> !default_env
    in
    Toploop.toplevel_env := typing_env;
    Value_env.restore value_env

  let capture_by_filename filename =
    let (previous_env, previous_values) =
      try Filename_map.find filename !file_environments
      with Not_found -> !default_env
    in
    let idents = Env.diff previous_env !Toploop.toplevel_env in
    let values = Value_env.capture previous_values idents in
    file_environments := Filename_map.add filename (!Toploop.toplevel_env, values) !file_environments

  let reset id =
    let rec go id = function
      | [] -> failwith ("no environment " ^ string_of_int id)
      | (id', _, _) :: xs when id' >= id && xs <> [] -> go id xs
      | ((_, typing_env, value_env) as x) :: xs ->
          Toploop.toplevel_env := typing_env;
          Value_env.restore value_env;
          x :: xs
    in
    environments := go id !environments

  let capture id =
    let values =
      match !environments with
      | [] -> invalid_arg "empty environment"
      | (_, previous_env, previous_values) :: _ ->
          let idents = Env.diff previous_env !Toploop.toplevel_env in
          Value_env.capture previous_values idents
    in
    environments := (id, !Toploop.toplevel_env, values) :: !environments
end

let setup_toplevel ?warnings () =
  let _ = JsooTop.initialize () in
  Sys.interactive := false;
  (* Configure compiler warnings if provided *)
  Option.iter (fun w ->
    try
      let _alerts = Warnings.parse_options false w in
      ()
    with exn ->
      Brr.Console.error [Jstr.of_string "Failed to parse warnings config:"; Jstr.of_string (Printexc.to_string exn)]
  ) warnings;
  Environment.init ()

let rec parse_use_file ~caml_ppf lex =
  let _at = lex.Lexing.lex_curr_pos in
  match !Toploop.parse_toplevel_phrase lex with
  | ok -> Ok ok :: parse_use_file ~caml_ppf lex
  | exception End_of_file -> []
  | exception err -> [ Error err ]

let ppx_rewriters = ref []

(* Type checking support for spec cells *)
let is_spec_content code =
  (* Check if code looks like val declarations *)
  let trimmed = String.trim code in
  String.length trimmed >= 3 && String.sub trimmed 0 3 = "val"

let parse_val_decls spec =
  (* Parse "val name : type" declarations from spec string *)
  let lines = String.split_on_char '\n' spec in
  List.filter_map (fun line ->
    let line = String.trim line in
    if String.length line = 0 then None
    else if String.length line >= 3 && String.sub line 0 3 = "val" then
      (* Match "val name : type" *)
      let rest = String.trim (String.sub line 3 (String.length line - 3)) in
      match String.index_opt rest ':' with
      | None -> None
      | Some colon_pos ->
          let name = String.trim (String.sub rest 0 colon_pos) in
          let typ = String.trim (String.sub rest (colon_pos + 1) (String.length rest - colon_pos - 1)) in
          if String.length name > 0 && String.length typ > 0 then Some (name, typ)
          else None
    else None
  ) lines

let check_type_constraint name expected_type =
  (* Try to compile: let (_ : expected_type) = name *)
  let code = Printf.sprintf "let (_ : %s) = %s ;;" expected_type name in
  let lexer = Lexing.from_string code in
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  try
    match !Toploop.parse_toplevel_phrase lexer with
    | phrase ->
        let success = Toploop.execute_phrase false ppf phrase in
        Format.pp_print_flush ppf ();
        if success then Ok ()
        else Error (Buffer.contents buf)
    | exception End_of_file -> Error "parse error"
    | exception exn ->
        Errors.report_error ppf exn;
        Format.pp_print_flush ppf ();
        Error (Buffer.contents buf)
  with exn ->
    Errors.report_error ppf exn;
    Format.pp_print_flush ppf ();
    Error (Buffer.contents buf)

let escape_html s =
  s |> String.split_on_char '&' |> String.concat "&amp;"
    |> String.split_on_char '<' |> String.concat "&lt;"
    |> String.split_on_char '>' |> String.concat "&gt;"

let run_type_checks specs =
  (* Run type checks and return HTML output *)
  let results = List.map (fun (name, typ) ->
    match check_type_constraint name typ with
    | Ok () ->
        Printf.sprintf "<div style=\"color: green;\">✓ %s : %s</div>"
          (escape_html name) (escape_html typ)
    | Error msg ->
        Printf.sprintf "<div style=\"color: red;\">✗ %s : %s<pre style=\"margin: 0.2em 0 0.5em 1em; font-size: 0.9em;\">%s</pre></div>"
          (escape_html name) (escape_html typ) (escape_html msg)
  ) specs in
  if results = [] then []
  else [Html (String.concat "" results)]

let execute_type_checks ~id:_ ?filename:_ code_text =
  (* Environment is already in the correct state after impl cell ran -
     the type check message is processed immediately after the impl completes *)
  Brr.Console.log ["execute_type_checks called with:"; code_text];
  let specs = parse_val_decls code_text in
  Brr.Console.log ["parsed specs count:"; string_of_int (List.length specs)];
  let result = run_type_checks specs in
  Brr.Console.log ["type check result count:"; string_of_int (List.length result)];
  result

let preprocess_structure str =
  let open Ast_mapper in
  List.fold_right
    (fun ppx_rewriter str ->
      let mapper = ppx_rewriter [] in
      mapper.structure mapper str)
    !ppx_rewriters str

let preprocess_phrase phrase =
  let open Parsetree in
  match phrase with
  | Ptop_def str -> Ptop_def (preprocess_structure str)
  | Ptop_dir _ as x -> x

let execute ~id ~line_number ~output ?filename code_text =
  (* Use filename-based environment if provided, otherwise use ID-based *)
  (match filename with
   | Some fname -> Environment.reset_by_filename fname
   | None -> Environment.reset id);
  let outputs = ref [] in
  let buf = Buffer.create 64 in
  let caml_ppf = Format.formatter_of_buffer buf in
  let content = code_text ^ " ;;" in
  let lexer = Lexing.from_string content in
  let pos_fname = Option.value ~default:"" filename in
  Lexing.set_position lexer
    { pos_fname; pos_lnum = line_number; pos_bol = 0; pos_cnum = 0 };
  let phrases = parse_use_file ~caml_ppf lexer in
  Js_of_ocaml.Sys_js.set_channel_flusher stdout (fun str ->
      outputs := Stdout str :: !outputs);
  Js_of_ocaml.Sys_js.set_channel_flusher stderr (fun str ->
      outputs := Stderr str :: !outputs);
  let get_out () =
    Format.pp_print_flush caml_ppf ();
    let meta = Buffer.contents buf in
    Buffer.clear buf;
    let out = if meta = "" then !outputs else Meta meta :: !outputs in
    outputs := [];
    List.rev out
  in
  let respond ~(at_loc : Location.t) =
    let loc = at_loc.loc_end.pos_cnum in
    let out = get_out () in
    output ~loc out
  in
  List.iter
    (function
      | Error err -> Errors.report_error caml_ppf err
      | Ok phrase ->
          let sub_phrases =
            match phrase with
            | Parsetree.Ptop_def s ->
                List.map (fun s -> Parsetree.Ptop_def [ s ]) s
            | Ptop_dir _ -> [ phrase ]
          in
          List.iter
            (fun phrase ->
              let at_loc =
                match phrase with
                | Parsetree.Ptop_def ({ pstr_loc = loc; _ } :: _) -> loc
                | Ptop_dir { pdir_loc = loc; _ } -> loc
                | _ -> assert false
              in
              X_ocaml_lib.id := (id, at_loc.loc_end.pos_cnum);
              try
                Location.reset ();
                let phrase = preprocess_phrase phrase in
                let _r = Toploop.execute_phrase true caml_ppf phrase in
                respond ~at_loc
              with _exn ->
                Errors.report_error caml_ppf _exn;
                respond ~at_loc)
            sub_phrases)
    phrases;
  (* Save environment state *)
  (match filename with
   | Some fname -> Environment.capture_by_filename fname
   | None -> Environment.capture id);
  get_out ()

let () =
  Ast_mapper.register_function :=
    fun _ f -> ppx_rewriters := f :: !ppx_rewriters
