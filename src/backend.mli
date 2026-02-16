(** Backend abstraction for x-ocaml.

    This module provides a unified client type that can use either the built-in
    x-ocaml worker or js_top_worker as the backend.

    Usage:
    {[
      (* Select backend from attribute *)
      let worker = Backend.make ~backend:"jtw" worker_url in

      (* Use unified API *)
      Backend.on_message worker (fun msg -> ...);
      Backend.eval ~id:1 ~line_number:1 worker "let x = 1";
    ]} *)

(** The backend module signature *)
module type S = sig
  type t

  (** Create a new backend client.
      @param extra_load Optional URL to load before the worker
      @param url URL to the worker script *)
  val make : ?extra_load:string -> string -> t

  (** Set the response handler. Called for each response from the worker. *)
  val on_message : t -> (X_protocol.response -> unit) -> unit

  (** Send a raw protocol message to the worker.
      Note: Some backends may not support all message types (e.g., Merlin). *)
  val post : t -> X_protocol.request -> unit

  (** Evaluate OCaml code.
      @param id Cell identifier
      @param line_number Starting line number
      @param t The client
      @param code OCaml code to evaluate *)
  val eval : id:int -> line_number:int -> t -> string -> unit

  (** Format OCaml code using ocamlformat.
      @param id Cell identifier
      @param t The client
      @param code OCaml code to format *)
  val fmt : id:int -> t -> string -> unit

  (** Whether this backend supports Merlin integration *)
  val has_merlin : bool
end

(** {1 Unified Client Type} *)

(** Unified client type that can hold any backend *)
type t

(** Check if the backend supports Merlin integration *)
val has_merlin : t -> bool

(** Create a client with the built-in x-ocaml worker backend *)
val make_builtin : ?extra_load:string -> string -> t

(** Create a client with the js_top_worker backend *)
val make_jtw : string -> t

(** Create a client with the specified backend.
    @param backend Backend name: "builtin", "x-ocaml", "jtw", or "js_top_worker"
    @param extra_load Optional URL to load before the worker (builtin only)
    @param url URL to the worker script *)
val make : backend:string -> ?extra_load:string -> string -> t

(** Set the response handler *)
val on_message : t -> (X_protocol.response -> unit) -> unit

(** Send a raw protocol message (no-op for jtw backend) *)
val post : t -> X_protocol.request -> unit

(** Evaluate OCaml code *)
val eval : id:int -> line_number:int -> t -> string -> unit

(** Format OCaml code *)
val fmt : id:int -> t -> string -> unit

(** {1 Module-based Interface} *)

(** Built-in x-ocaml worker backend module *)
module Builtin_mod : S

(** js_top_worker backend module *)
module Jtw_mod : S

(** Select backend module by name.
    @param name "builtin" or "jtw"
    @return The backend module *)
val select : string -> (module S)
