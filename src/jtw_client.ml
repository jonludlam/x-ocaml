(** Bridge between x-ocaml's X_protocol and js_top_worker's JSON-RPC protocol.

    This module translates X_protocol requests into js_top_worker RPC calls
    and converts the results back into X_protocol responses. *)

open Brr
module Jtw = Js_top_worker_client_fut
module W = Jtw.W
module Api = Js_top_worker_rpc.Toplevel_api_gen

type t = {
  rpc : Jtw.rpc;
  mutable on_message_cb : X_protocol.response -> unit;
}

let make url =
  let timeout_fn () =
    Brr.Console.(log [ str "js_top_worker: timeout" ])
  in
  let rpc = Jtw.start url 30_000 timeout_fn in
  { rpc; on_message_cb = (fun _ -> ()) }

let on_message t fn = t.on_message_cb <- fn

(** Send a response back to x-ocaml via the stored callback. *)
let respond t resp = t.on_message_cb resp

(** Convert a merlin position (polymorphic variant) to js_top_worker's
    msource_position (regular variant). *)
let convert_position
    (pos : [ `Start | `Offset of int | `Logical of int * int | `End ]) :
    Api.msource_position =
  match pos with
  | `Start -> Api.Start
  | `Offset n -> Api.Offset n
  | `Logical (line, col) -> Api.Logical (line, col)
  | `End -> Api.End

(** Convert js_top_worker kind_ty to merlin's Query_protocol.Compl.entry kind *)
let convert_kind (k : Api.kind_ty) :
    [ `Value
    | `Constructor
    | `Variant
    | `Label
    | `Module
    | `Modtype
    | `Type
    | `MethodCall
    | `Keyword ] =
  match k with
  | Api.Value -> `Value
  | Api.Constructor -> `Constructor
  | Api.Variant -> `Variant
  | Api.Label -> `Label
  | Api.Module -> `Module
  | Api.Modtype -> `Modtype
  | Api.Type -> `Type
  | Api.MethodCall -> `MethodCall
  | Api.Keyword -> `Keyword

(** Convert js_top_worker completion entry to merlin's Query_protocol.Compl.entry *)
let convert_compl_entry (e : Api.query_protocol_compl_entry) :
    Query_protocol.Compl.entry =
  {
    Query_protocol.Compl.name = e.Api.name;
    kind = convert_kind e.Api.kind;
    desc = e.Api.desc;
    info = e.Api.info;
    deprecated = e.Api.deprecated;
  }

(** Convert js_top_worker completions to Protocol.completions *)
let convert_completions (c : Api.completions) : Protocol.completions =
  {
    Protocol.from = c.Api.from;
    to_ = c.Api.to_;
    entries = List.map convert_compl_entry c.Api.entries;
  }

(** Convert js_top_worker error to Protocol.error.
    Both use Ocaml_parsing.Location types, so this is a direct mapping. *)
let convert_error (e : Api.error) : Protocol.error =
  {
    Protocol.kind = e.Api.kind;
    loc = e.Api.loc;
    main = e.Api.main;
    sub = e.Api.sub;
    source = e.Api.source;
  }

(** Convert js_top_worker is_tail_position to Protocol.is_tail_position *)
let convert_tail_position (tp : Api.is_tail_position) :
    Protocol.is_tail_position =
  match tp with
  | Api.No -> `No
  | Api.Tail_position -> `Tail_position
  | Api.Tail_call -> `Tail_call

(** Convert js_top_worker index_or_string to the polymorphic variant form *)
let convert_index_or_string (ios : Api.index_or_string) :
    [ `Index of int | `String of string ] =
  match ios with
  | Api.Index i -> `Index i
  | Api.String s -> `String s

(** Convert a js_top_worker typed_enclosings entry to Protocol format *)
let convert_typed_enclosing
    ((loc, ios, tp) : Api.typed_enclosings) :
    Ocaml_parsing.Location.t
    * [ `Index of int | `String of string ]
    * Protocol.is_tail_position =
  (loc, convert_index_or_string ios, convert_tail_position tp)

(** Convert exec_result to X_protocol output list *)
let convert_exec_result (r : Api.exec_result) : X_protocol.output list =
  let outputs = ref [] in
  (* Add sharp_ppf output (toplevel responses like "val x : int = 1") *)
  (match r.Api.sharp_ppf with
   | Some s when s <> "" -> outputs := X_protocol.Meta s :: !outputs
   | _ -> ());
  (* Add caml_ppf output (type/module signatures) *)
  (match r.Api.caml_ppf with
   | Some s when s <> "" -> outputs := X_protocol.Meta s :: !outputs
   | _ -> ());
  (* Add stdout *)
  (match r.Api.stdout with
   | Some s when s <> "" -> outputs := X_protocol.Stdout s :: !outputs
   | _ -> ());
  (* Add stderr *)
  (match r.Api.stderr with
   | Some s when s <> "" -> outputs := X_protocol.Stderr s :: !outputs
   | _ -> ());
  List.rev !outputs

(** Ignore errors from async operations, logging them to the console *)
let handle_error = function
  | Ok v -> Some v
  | Error (Api.InternalError _msg) ->
    Console.(log [ str "jtw_client error:"; str _msg ]);
    None

let init t =
  let open Fut.Syntax in
  let config : Api.init_config =
    {
      findlib_requires = [];
      stdlib_dcs = None;
      findlib_index = None;
      execute = true;
    }
  in
  let _fut : unit Fut.t =
    let* result = W.init t.rpc config in
    (match result with
     | Ok () -> ()
     | Error (Api.InternalError _msg) ->
       Console.(log [ str "jtw_client init error:"; str _msg ]));
    Fut.return ()
  in
  ()

let post t (req : X_protocol.request) =
  let open Fut.Syntax in
  match req with
  | X_protocol.Eval (id, line_number, code) ->
    let _fut : unit Fut.t =
      let* result = W.exec t.rpc "" code in
      (match handle_error result with
       | Some exec_result ->
         let outputs = convert_exec_result exec_result in
         if line_number > 0 then
           respond t (X_protocol.Top_response_at (id, line_number, outputs))
         else
           respond t (X_protocol.Top_response (id, outputs))
       | None ->
         respond t
           (X_protocol.Top_response
              (id, [ X_protocol.Stderr "Internal error during evaluation" ])));
      Fut.return ()
    in
    ()
  | X_protocol.Merlin (id, Protocol.Complete_prefix (src, pos)) ->
    let jtw_pos = convert_position pos in
    let _fut : unit Fut.t =
      let* result =
        W.complete_prefix t.rpc "" None [] false src jtw_pos
      in
      (match handle_error result with
       | Some completions ->
         let converted = convert_completions completions in
         respond t
           (X_protocol.Merlin_response (id, Protocol.Completions converted))
       | None ->
         respond t
           (X_protocol.Merlin_response
              (id,
               Protocol.Completions
                 { Protocol.from = 0; to_ = 0; entries = [] })));
      Fut.return ()
    in
    ()
  | X_protocol.Merlin (id, Protocol.Type_enclosing (src, pos)) ->
    let jtw_pos = convert_position pos in
    let _fut : unit Fut.t =
      let* result =
        W.type_enclosing t.rpc "" None [] false src jtw_pos
      in
      (match handle_error result with
       | Some enclosings ->
         let converted = List.map convert_typed_enclosing enclosings in
         respond t
           (X_protocol.Merlin_response
              (id, Protocol.Typed_enclosings converted))
       | None ->
         respond t
           (X_protocol.Merlin_response
              (id, Protocol.Typed_enclosings [])));
      Fut.return ()
    in
    ()
  | X_protocol.Merlin (id, Protocol.All_errors src) ->
    let _fut : unit Fut.t =
      let* result =
        W.query_errors t.rpc "" None [] false src
      in
      (match handle_error result with
       | Some errors ->
         let converted = List.map convert_error errors in
         respond t
           (X_protocol.Merlin_response (id, Protocol.Errors converted))
       | None ->
         respond t
           (X_protocol.Merlin_response (id, Protocol.Errors [])));
      Fut.return ()
    in
    ()
  | X_protocol.Merlin (id, Protocol.Add_cmis _) ->
    (* js_top_worker handles CMI loading internally via its init config *)
    respond t (X_protocol.Merlin_response (id, Protocol.Added_cmis))
  | X_protocol.Format (id, code) ->
    (* js_top_worker doesn't support formatting; return the code as-is *)
    respond t (X_protocol.Formatted_source (id, code))
  | X_protocol.Format_config _ ->
    (* No-op: js_top_worker doesn't support format configuration *)
    ()
  | X_protocol.Setup ->
    init t

let eval ~id ~line_number t code =
  post t (X_protocol.Eval (id, line_number, code))

let fmt ~id t code =
  post t (X_protocol.Format (id, code))
