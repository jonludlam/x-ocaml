(** How much work does it do? Count it: rewrite the code so that it counts
    every entry to a function's body and every [::] it builds, and give it
    counting versions of [List] and [( @ )], which charge a step for each
    element they go through or build. Counts are exact, so they can be
    compared across sizes (see {!Growth}) without the noise of timing. *)

val instrument : Parsetree.structure -> Parsetree.structure
(** Every function body, and every case of a [function], starts with a step;
    every [::] built is one. *)

val prelude : Parsetree.structure
(** Defines [Focs_count] (the counter, and [Out_of_fuel]), [( @ )], and a
    [List] whose functions count. *)

val load : name:string -> Parsetree.structure -> Isolated.loaded
(** Loads the structure, instrumented, after {!prelude}, as module [name]. *)

val measure :
  name:string ->
  ?fuel:int ->
  input:string ->
  call:string ->
  int list ->
  (int * int option) list
(** [measure ~name ~input ~call sizes] runs [call (input n)] for each size
    [n], and counts the steps [call] takes: [input] (an OCaml expression of
    type [int -> 'a]) is not counted, being outside [name]. [call] is an
    expression of type ['a -> 'b], which uses module [name], loaded by
    {!load}. A run that takes more than [fuel] steps (by default ten
    million) is stopped, and counts [None]. Raises [Failure] if [input] or
    [call] does not type-check. *)
