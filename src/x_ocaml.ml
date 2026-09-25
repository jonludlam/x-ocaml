let current_script =
  Brr.El.of_jv (Jv.get (Brr.Document.to_jv Brr.G.document) "currentScript")

let current_attribute attr = Brr.El.at (Jstr.of_string attr) current_script
let current_string attr = Option.map Jstr.to_string (current_attribute attr)

let worker_url =
  match current_string "src-worker" with
  | None -> failwith "x-ocaml script missing src-worker attribute"
  | Some url -> url

let page =
  Page.create
    ~worker:(Client.make ?extra_load:(current_string "src-load") worker_url)
    ~worker_url
    ?extra_style:(current_attribute "src-style")
    ?inline_style:(current_attribute "inline-style")
    ?default_run_on:(current_string "run-on")
    ?format_config:(current_string "x-ocamlformat")
    ()

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.of_string "x-ocaml"
  | Some name -> name

let _ =
  Webcomponent.define elt_name @@ fun this -> ignore (Page.register page this)
