(** Checking an exercise's code from a test cell: reading it, and loading it
    apart from the cells, rewritten as a check needs, with what the compiler
    said about it.

    {[
      let s = Focs_check.load ~as_:"Student" "rev-exercise"
      let () = assert s.ok
      ;;
      assert (Student.rev [ 1; 2; 3 ] = [ 3; 2; 1 ])
    ]}

    A module loaded is there for the cell's later phrases, not the phrase
    that loads it, which was type-checked before it ran. *)

(** {1 Code} *)

val source : string -> string
(** [source name]: the code of the cell whose [data-id] is [name], as it was
    last run. Raises [Failure] if no such cell has run. *)

(** {1 Diagnostics} *)

type diagnostic = {
  line : int;  (** 0 if the diagnostic has no location *)
  first : int;  (** the characters, on the line, from first to last *)
  last : int;
  warning : int option;  (** the warning's number, if it is one *)
  error : bool;  (** an error, or a warning made an error *)
  message : string;
}

val string_of_diagnostic : diagnostic -> string

(** {1 Loading} *)

type loaded = {
  name : string;
  ok : bool;  (** it type-checked, and running it raised nothing *)
  diagnostics : diagnostic list;
}

val load : ?warnings:string -> as_:string -> string -> loaded
(** [load ~as_:"M" name] loads the code of the cell [name] as a module [M].
    [warnings] are flags as for [-w], after the defaults. Lines are the
    page's, as the cell shows them. *)

val load_code : ?warnings:string -> ?line:int -> as_:string -> string -> loaded
(** [load_code ~as_:"M" code] loads [code] itself. *)

val print_diagnostics : loaded -> unit
(** On stdout, one a line. *)

(** {1 Tail calls}

    Every call to a function of its enclosing [let rec] group is marked
    [[@tailcall]], and the compiler says which are not tail calls (see
    Focs_check_core.Tailcheck). *)

type call = { caller : string; callee : string; line : int }

type tail = {
  calls : call list;  (** every recursive call *)
  not_tail : call list;  (** those not in tail position *)
  tail_loaded : loaded;  (** the code, loaded as it was checked *)
}

val tailcheck : ?as_:string -> string -> tail
(** [tailcheck name] checks the code of the cell [name]. *)

val tailcheck_code : ?line:int -> ?as_:string -> string -> tail
val is_tail_recursive : tail -> bool
(** It has recursive calls, and all are tail calls. *)

val print_tail : tail -> unit
(** What a reader is told: each call that is not a tail call, and where. *)

(** {1 Work}

    The code is loaded rewritten to count the steps it takes (see
    Focs_check_core.Count): each entry to a function's body, each [::] it
    builds, and each element that [List]'s functions and [( @ )] go
    through. *)

val load_counting : as_:string -> string -> loaded
(** [load_counting ~as_:"M" name] loads the cell [name]'s code, counting, as
    [M]. *)

val load_counting_code : ?line:int -> as_:string -> string -> loaded

val measure :
  ?fuel:int ->
  as_:string ->
  input:string ->
  call:string ->
  int list ->
  (int * int option) list
(** [measure ~as_:"M" ~input:"fun n -> List.init n Fun.id" ~call:"M.rev"
    sizes]: for each size, the steps [call (input n)] took, or [None] if it
    took more than [fuel] (ten million). [input] and [call] are OCaml
    expressions, the first of type [int -> 'a], not counted, the second of
    type ['a -> 'b]. Raises [Failure] if they do not type-check. *)

type growth = {
  model : string;  (** ["O(1)"], ["O(log n)"], ["O(n)"], ["O(n log n)"], ["O(n^2)"], ["O(n^3)"] or ["O(2^n)"] *)
  explanation : string;
      (** "when the input doubled, it did about 4.0 times the work: O(n^2)" *)
  table : string;  (** the sizes, steps, and each ratio to the one before *)
}

val growth : (int * int option) list -> growth
(** Raises [Failure] if a run ran out of fuel, or there are fewer than three
    sizes. *)

val print_growth : growth -> unit

(** {1 What it uses}

    Checked on the typed code, where names are resolved: [module L = List]
    and then [L.rev] is a use of [Stdlib.List.rev]. *)

type construct = While | For | Let_rec | Recursion | Mutation | Try | Array

type rule =
  | Forbid_value of string  (** by full path: ["Stdlib.List.rev"], ["Stdlib.@"] *)
  | Forbid_module of string  (** ["Stdlib.Array"] *)
  | Forbid of construct
  | Require of construct
  | Require_value of string

val check_uses : rule list -> string -> (int * string) list
(** [check_uses rules name]: the rules the cell [name]'s code breaks, each
    with its line (0 for the code as a whole) and a message. Raises
    [Failure] if the code does not type-check. *)

val check_uses_code : ?line:int -> rule list -> string -> (int * string) list

val uses : string -> string list
(** The values the cell's code uses from outside it, by full path. *)

val recursive_functions : string -> string list
(** The functions the cell's code defines that call themselves or each other. *)
