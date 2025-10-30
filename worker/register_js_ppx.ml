(* Register js_of_ocaml-ppx for use in the toplevel *)

(* Force the module to be loaded by referencing something from it *)
let () = ignore Ppx_js.mapper
