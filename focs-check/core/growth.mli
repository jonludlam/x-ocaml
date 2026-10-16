(** Which growth do the counts follow? Fit them against the usual models, as
    [a * g n + b], by least squares, and take the model whose fit is
    closest, preferring the slower-growing when two fit about as well.
    Exponential growth needs small sizes stepped by one. *)

type model = Constant | Log | Linear | N_log_n | Quadratic | Cubic | Exponential

val models : model list
val name : model -> string
(** As "O(n log n)". *)

type fit = {
  model : model;
  a : float;
  b : float;
  error : float;  (** the largest relative error over the points *)
}

type verdict = {
  best : model;
  fits : fit list;  (** every model's, best first *)
  ratios : (int * float) list;
      (** for each size after the first: its count over the previous one *)
}

val classify : (int * int) list -> verdict
(** Needs at least three sizes, increasing. *)

val table : (int * int) list -> string
(** The counts as a table, with the ratio of each to the one before, and
    what that ratio says when the size doubled: the feedback a reader sees. *)

val explain : verdict -> string
(** One sentence, from the last two sizes: "when the input doubled, it did
    about 4.0 times the work: O(n^2)". *)
