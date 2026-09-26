open Brr

let define tag ~on_data ~on_disconnect =
  let make =
    Jv.call Jv.global "eval"
      [| Jv.of_string
           {|(function (onData, onGone) {
               return class extends HTMLElement {
                 set data(s) { this._data = s; onData(this, s); }
                 get data() { return this._data; }
                 disconnectedCallback() { onGone(this); }
               };
             })|} |]
  in
  let cls =
    Jv.apply make
      [| Jv.callback ~arity:2 (fun el s -> on_data (El.of_jv el) (Jv.to_string s));
         Jv.callback ~arity:1 (fun el -> on_disconnect (El.of_jv el)) |]
  in
  ignore (Jv.call (Jv.get Jv.global "customElements") "define" [| Jv.of_string tag; cls |])

let callback el ~token payload =
  let detail = Jv.obj [| ("token", Jv.of_int token); ("payload", Jv.of_string payload) |] in
  let ev =
    Jv.new' (Jv.get Jv.global "CustomEvent")
      [| Jv.of_string "x-ocaml-callback"; Jv.obj [| ("bubbles", Jv.true'); ("detail", detail) |] |]
  in
  ignore (Jv.call (El.to_jv el) "dispatchEvent" [| ev |])
