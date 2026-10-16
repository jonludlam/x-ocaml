(** Is it tail recursive? Ask the compiler: mark every call to a function of
    its enclosing [let rec] group [[@tailcall]], and see which draw warning
    51. This is the compiler's own idea of a tail call: it knows about
    [try ... with], sequences, the branches of [match] and [if], and mutual
    recursion. Recursion through other functions ([List.iter f]) is not
    seen. *)

type call = {
  caller : string;  (** the function the call is in *)
  callee : string;
  loc : Location.t;  (** of the application *)
}

val annotate : Parsetree.structure -> Parsetree.structure * call list
(** The structure with the recursive calls marked, and the calls. The
    code's own warning attributes are removed, so that it cannot silence
    the check. *)

type result = {
  calls : call list;  (** every recursive call *)
  not_tail : call list;  (** the ones that are not tail calls *)
  loaded : Isolated.loaded;
}

val check : ?prelude:Parsetree.structure -> name:string -> Parsetree.structure -> result
(** Loads the structure, annotated, as module [name]; [not_tail = []] if it
    is tail recursive (or has no recursive calls: see [calls]). *)
