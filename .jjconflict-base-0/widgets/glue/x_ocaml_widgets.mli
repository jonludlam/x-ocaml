(** js_top_worker's [Widget] library on x-ocaml's displays.

    Linked into a worker, this sets [Widget]'s sender: each widget becomes a
    display of an [<x-ocaml-view>] element, defined by the page half,
    [x-ocaml-widgets.js], which the page loads from beside the worker when
    the first widget is shown. When a widget's cell runs again, its display
    goes and its handler ["x-ocaml:removed"], if it has one, is called. *)
