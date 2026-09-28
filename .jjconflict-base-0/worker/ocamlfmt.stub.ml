(* Without ocamlformat-lib (which cannot be installed alongside
   js_of_ocaml-compiler on OxCaml), formatting leaves the code as it is. *)

let configure _ = ()
let fmt source = source
