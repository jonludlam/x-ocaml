// A workaround for js_of_ocaml: OCaml 5's bytecode compiler calls
// caml_ensure_stack_capacity from code with a large frame, as a module of a
// dozen or so definitions has. js_of_ocaml compiles the call to nothing,
// and its runtime has no such function; built with separate compilation
// (dune's dev profile), its toplevel then refuses such a cell with "The
// external function caml_ensure_stack_capacity is not available". This stub
// can go once js_of_ocaml's toplevel knows the primitives it compiles away.

//Provides: caml_ensure_stack_capacity
function caml_ensure_stack_capacity(_n) {
  return 0;
}
