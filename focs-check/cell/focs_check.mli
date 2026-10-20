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
  | Forbid_library of string list
      (** No value from the standard library's modules however reached
          (aliases, [open], [Stdlib.List.fold_left]), except from those
          listed (["Stdlib.Printf"]); what is open by default ([+], [@],
          [failwith]) stays allowed, unless written [Stdlib.]. *)

val check_uses : rule list -> string -> (int * string) list
(** [check_uses rules name]: the rules the cell [name]'s code breaks, each
    with its line (0 for the code as a whole) and a message. Raises
    [Failure] if the code does not type-check. *)

val check_uses_code : ?line:int -> rule list -> string -> (int * string) list

val uses : string -> string list
(** The values the cell's code uses from outside it, by full path. *)

val recursive_functions : string -> string list
(** The functions the cell's code defines that call themselves or each other. *)

(** {1 Steps to predict}

    A computation's states, shown one at a time by an [<x-stepper>] element
    (examples/elements/stepper.js) as box-and-pointer diagrams: lists as
    chains of cells, trees as trees, and the cells two values share drawn
    once, with two arrows to them. Sharing is found by physical equality, so
    it is the program's own. In predict mode the reader chooses the next
    state among it and wrong ones, each wrong one with what the
    misconception behind it gets wrong. *)

module Steps : sig
  type value

  val list : ('a -> string) -> 'a list -> value
  val tree : ('a -> string) -> ('t -> ('a * 't list) option) -> 't -> value
  (** [tree label view t]: [view] says what a node holds and its children,
      or [None] for an empty tree. *)

  val text : string -> value
  (** A plain value, written out. *)

  type state

  val state :
    ?label:string -> ?note:string -> ?line:int -> ?highlight:value list ->
    (string * value) list -> state
  (** The named values; [highlight] marks the first node of each value, and
      [line] the line of the code (see [show]) that runs next. *)

  type t

  val create : unit -> t

  val add : t -> ?choices:(string * state) list -> state -> unit
  (** The next state, with wrong ones to choose among: [(why, state)]. *)

  val json : ?id:string -> ?title:string -> ?code:string -> ?mode:string -> ?ask:int -> t -> string
  (** As [<x-stepper>] takes it; [mode] is ["predict"] (by default) or
      ["lecture"], and [ask] how many steps to ask about before only the
      last (2). *)

  val show :
    ?src:string -> ?id:string -> ?title:string -> ?code:string -> ?mode:string -> ?ask:int ->
    t -> unit
  (** Shows the steps under the cell, with [code], if given, beside them and
      each state's [line] of it marked. [src] is where the element's script
      is, relative to the worker (examples/elements/stepper.js). *)
end

(** {1 Examples first}

    As in Examplar: the reader writes examples of what a function should
    do; each must agree with hidden correct versions (wheats), and a good
    set disagrees with each wrong version (chaff), each made from a
    misconception (see Focs_check_core.Examplar). *)

type examplar = {
  valid : int;  (** examples the wheats agree with *)
  invalid : int;
  caught : int;  (** chaffs caught by a valid example *)
  chaffs : int;
}

val examplar :
  wheats:string list -> chaffs:(string * string) list -> string -> examplar
(** [examplar ~wheats ~chaffs name] runs the examples in the cell [name]
    against each wheat (its code) and each chaff ([(what it gets wrong,
    code)]), and prints what the reader is told: each example and whether
    it is valid, the chaffs caught, by what they get wrong, and "caught N of
    M". The examples are a cell's phrases, each [assert e] or a boolean
    [e], one a line; or [let examples = [ e1; e2 ]]. *)
