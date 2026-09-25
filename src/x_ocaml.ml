let current_script =
  Brr.El.of_jv (Jv.get (Brr.Document.to_jv Brr.G.document) "currentScript")

let current_attribute attr = Brr.El.at (Jstr.of_string attr) current_script
let current_string attr = Option.map Jstr.to_string (current_attribute attr)

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.of_string "x-ocaml"
  | Some name -> name

let start worker_url =
  let page =
    Page.create
      ~worker:(Client.make ?extra_load:(current_string "src-load") worker_url)
      ~worker_url
      ?extra_style:(current_attribute "src-style")
      ?inline_style:(current_attribute "inline-style")
      ?default_run_on:(current_string "run-on")
      ?format_config:(current_string "x-ocamlformat")
      ()
  in
  ignore
  @@ Webcomponent.define elt_name
  @@ fun this -> ignore (Page.register page this)

(* Without a worker nothing can run: say so, and leave the cells as the
   text they are. *)
let () =
  match current_string "src-worker" with
  | Some url -> start url
  | None ->
      Brr.Console.error
        [ Jstr.v "x-ocaml: the script needs a src-worker attribute" ]
