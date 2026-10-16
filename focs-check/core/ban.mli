(** What may the code use? Checked on the typed tree, where every name has
    been resolved to the path it stands for: [module L = List] and then
    [L.rev], or [let r = List.rev], is still a use of [Stdlib.List.rev],
    as it would not be to a check on the code as written. *)

type construct =
  | While
  | For
  | Let_rec  (** a [let rec] *)
  | Recursion  (** a [let rec] whose functions call themselves, or each other *)
  | Mutation  (** [:=], [<-], or a mutable field or array *)
  | Try  (** [try ... with], or [match ... with exception] *)
  | Array  (** an array literal *)

type rule =
  | Forbid_value of string
      (** A value, by its full path: ["Stdlib.List.rev"], ["Stdlib.@"]. *)
  | Forbid_module of string
      (** Every value of a module: ["Stdlib.Array"]. *)
  | Forbid of construct
  | Require of construct
  | Require_value of string

type violation = { rule : rule; loc : Location.t; message : string }

val uses : Typedtree.structure -> (string * Location.t) list
(** Every value the code uses from outside it, by its full path. *)

val recursive_functions : Typedtree.structure -> string list
(** The functions defined by [let rec] that call themselves or each other. *)

val check : rule list -> Typedtree.structure -> violation list
