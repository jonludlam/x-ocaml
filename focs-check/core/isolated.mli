(** Code checked and run apart from the cells: parsed, rewritten,
    type-checked and evaluated as a module of its own in the toplevel, with
    what the compiler said kept rather than printed. *)

(** {1 Diagnostics} *)

type kind =
  | Error
  | Warning of { number : int; as_error : bool }
      (** [as_error]: the warning flags make it an error. The toplevel goes
          on regardless after some (warning 51 comes after typing), so a
          check reads it here rather than counting on the code failing. *)

type diagnostic = { kind : kind; loc : Location.t; message : string }

val pp_diagnostic : Format.formatter -> diagnostic -> unit
(** As [Line l, characters a-b: Warning 51: expected tailcall]. *)

val warnings : int -> diagnostic list -> diagnostic list
(** The diagnostics that are warning [n]. *)

val errors : diagnostic list -> diagnostic list
(** The errors, and the warnings made errors. *)

(** {1 Stages} *)

val parse :
  ?fname:string ->
  ?line:int ->
  string ->
  (Parsetree.structure, diagnostic list) result
(** [parse ~line code], where [code] starts at line [line] (by default 1), for
    the diagnostics' locations. *)

val typecheck :
  ?warnings:string ->
  ?env:Env.t ->
  Parsetree.structure ->
  (Typedtree.structure, unit) result * diagnostic list
(** Type-checks the structure in [env], by default the toplevel's, without
    changing it. [warnings] is as for [-w], applied after the defaults. *)

type loaded = {
  name : string;  (** of the module, as later phrases name it *)
  ok : bool;  (** it type-checked, and evaluating it raised nothing *)
  diagnostics : diagnostic list;
  exn : exn option;  (** what evaluating it raised, if it did *)
}

val load :
  ?warnings:string ->
  ?prelude:Parsetree.structure ->
  name:string ->
  Parsetree.structure ->
  loaded
(** [load ~name str] evaluates [module Name = struct prelude str end] in the
    toplevel. The prelude is not the reader's: what it defines can shadow
    the standard library for them ([List], [( @ )]), and its diagnostics
    are not theirs, and are dropped. Then later phrases see [Name], and
    {!value} can use it. *)

val value : ?loc:Location.t -> typ:string -> string -> Obj.t
(** [value ~typ e] evaluates the expression [e] at the type [typ] in the
    toplevel's environment, where {!load}ed modules are, and gives the
    value, which is of type [typ]: the caller then [Obj.obj]s it at that
    type. Raises [Failure] with the diagnostics if [e] does not type-check
    at [typ]. *)

(** {1 For the worker} *)

val reinstate : unit -> unit
(** Adds the modules {!load} defined back into the toplevel's environment. A
    phrase that loads a module, from within the toplevel, ends by setting
    the environment to the one it computed beforehand, which does not have
    it: the worker calls this after each phrase. *)

val prelude : string -> Parsetree.structure
(** Parses code for {!load}'s [prelude]. Raises [Failure] if it does not
    parse. *)
