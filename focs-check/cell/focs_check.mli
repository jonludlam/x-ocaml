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
