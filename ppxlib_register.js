(function(globalThis){
  "use strict";
   var runtime = globalThis.jsoo_runtime;
   var register_global = runtime.caml_register_global;
   runtime.caml_register_global = function (a,b,c) {
     if (c !== 'Ast_mapper') {
       return register_global(a,b,c);
     }
   };
   var create_file = runtime.jsoo_create_file;
   runtime.jsoo_create_file = function(a,b) {
     try {
       return create_file(a,b);
     } catch(_err) {
      // console.log('jsoo_create_file', a, err);
     }
   };
}
(globalThis));