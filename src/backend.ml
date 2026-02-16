(** Backend abstraction for x-ocaml.

    This module provides a unified client type that can use either the built-in
    x-ocaml worker or js_top_worker as the backend. *)

module type S = sig
  type t
  val make : ?extra_load:string -> string -> t
  val on_message : t -> (X_protocol.response -> unit) -> unit
  val post : t -> X_protocol.request -> unit
  val eval : id:int -> line_number:int -> t -> string -> unit
  val fmt : id:int -> t -> string -> unit
  val has_merlin : bool
end

(** Unified client type that works with any backend *)
type t =
  | Builtin of Client.t
  | Jtw of Jtw_client.t

let has_merlin = function
  | Builtin _ -> true
  | Jtw _ -> true  (* js_top_worker has complete, type_at, and errors *)

let make_builtin ?extra_load url =
  Builtin (Client.make ?extra_load url)

let make_jtw url =
  let client = Jtw_client.make url in
  Jtw_client.init client;
  Jtw client

let make ~backend ?extra_load url =
  match String.lowercase_ascii backend with
  | "jtw" | "js_top_worker" -> make_jtw url
  | "builtin" | "x-ocaml" | _ -> make_builtin ?extra_load url

let on_message t fn =
  match t with
  | Builtin client -> Client.on_message client fn
  | Jtw client -> Jtw_client.on_message client fn

let post t msg =
  match t with
  | Builtin client -> Client.post client msg
  | Jtw client -> Jtw_client.post client msg

let eval ~id ~line_number t code =
  match t with
  | Builtin client -> Client.eval ~id ~line_number client code
  | Jtw client -> Jtw_client.eval ~id ~line_number client code

let fmt ~id t code =
  match t with
  | Builtin client -> Client.fmt ~id client code
  | Jtw client -> Jtw_client.fmt ~id client code

(** Module-based interface for advanced use cases *)

module Builtin_mod : S = struct
  type nonrec t = Client.t
  let make = Client.make
  let on_message = Client.on_message
  let post = Client.post
  let eval = Client.eval
  let fmt = Client.fmt
  let has_merlin = true
end

module Jtw_mod : S = struct
  type nonrec t = Jtw_client.t

  let make ?extra_load:_ url =
    let t = Jtw_client.make url in
    Jtw_client.init t;
    t

  let on_message = Jtw_client.on_message

  let post = Jtw_client.post

  let eval = Jtw_client.eval

  let fmt = Jtw_client.fmt

  let has_merlin = true  (* js_top_worker has complete, type_at, and errors *)
end

let select name =
  match String.lowercase_ascii name with
  | "jtw" | "js_top_worker" -> (module Jtw_mod : S)
  | "builtin" | "x-ocaml" | _ -> (module Builtin_mod : S)
