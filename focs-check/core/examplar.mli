(** Examples first, as in Examplar (Wrenn and Krishnamurthi): the reader
    writes examples of what a function should do, before writing it; each
    is run against hidden correct implementations (wheats), which a valid
    example must agree with, and wrong ones (chaffs), each from a
    misconception, which a good set of examples catches. *)

type impl = { name : string; about : string; code : string }

type example = {
  source : string;  (** as written *)
  line : int;
  valid : bool;  (** every wheat passes it *)
  wheats_failing : string list;
  catches : string list;  (** the chaffs it fails, if valid *)
  diagnostics : Isolated.diagnostic list;  (** what the compiler said, against the first wheat *)
}

type result = {
  examples : example list;
  caught : impl list;  (** the chaffs some valid example catches *)
  missed : impl list;
}

val run : wheats:impl list -> chaffs:impl list -> ?line:int -> string -> result
(** [run ~wheats ~chaffs code] runs each example in [code] against each
    implementation, each in a module of its own after the
    implementation's code. Each phrase of [code] is an example: [assert e],
    or a boolean expression [e], which must hold. A line that starts at its
    first column starts a phrase, and indented lines go on with it, so that
    examples need no [;;] between them. And [let examples = [ e1; e2 ]] is
    the examples [e1] and [e2], each on its own: the form that runs in a
    cell, which shows [val examples : bool list = [true; true]]. *)
