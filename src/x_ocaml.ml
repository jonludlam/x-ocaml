open Brr

let current_script =
  El.of_jv (Jv.get (Document.to_jv G.document) "currentScript")

let current_attribute attr = El.at (Jstr.of_string attr) current_script
let current_string attr = Option.map Jstr.to_string (current_attribute attr)

let meta name =
  let sel = Jv.of_string ("meta[name=\"" ^ name ^ "\"]") in
  Jv.to_option El.of_jv
    (Jv.call (Document.to_jv G.document) "querySelector" [| sel |])
  |> Fun.flip Option.bind (El.at (Jstr.v "content"))
  |> Option.map Jstr.to_string

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.of_string "x-ocaml"
  | Some name -> name

let resolve ?base url =
  let base =
    match base with
    | Some b -> Jv.of_string b
    | None -> Jv.get (Document.to_jv G.document) "baseURI"
  in
  Jv.to_string
    (Jv.get
       (Jv.new' (Jv.get Jv.global "URL") [| Jv.of_string url; base |])
       "href")

let start backend =
  let page =
    Page.create ~worker:backend
      ?extra_style:(current_attribute "src-style")
      ?inline_style:(current_attribute "inline-style")
      ?default_run_on:(current_string "run-on")
      ?format_config:(current_string "x-ocamlformat")
      ()
  in
  ignore
  @@ Webcomponent.define elt_name
  @@ fun this -> ignore (Page.register page this)

let error msg = Console.error [ Jstr.v ("x-ocaml: " ^ msg) ]

(* A universe's findlib_index.json names the worker that goes with it. *)
let start_universe base =
  let base = if String.ends_with ~suffix:"/" base then base else base ^ "/" in
  let index = resolve (base ^ "findlib_index.json") in
  let open Fut.Result_syntax in
  let fetched =
    let* response = Brr_io.Fetch.url (Jstr.v index) in
    let* text =
      Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body response)
    in
    Fut.return (Brr.Json.decode text)
  in
  Fut.await fetched @@ function
  | Error e ->
      error ("cannot read " ^ index ^ ": " ^ Jstr.to_string (Jv.Error.message e))
  | Ok json -> (
      let worker = Jv.find_path json [ "compiler"; "worker_url" ] in
      match (Option.map Jv.to_string worker, current_string "src-worker") with
      | None, None ->
          error (index ^ " names no worker, and the script gives none")
      | Some w, _ | None, Some w ->
          start (Backend.jtw ~findlib_index:index (resolve ~base:index w)))

(* Without a worker nothing can run: say so, and leave the cells as the
   text they are. *)
let () =
  match (meta "x-ocaml-universe", current_string "src-worker") with
  | Some universe, _ -> start_universe universe
  | None, Some url -> (
      match current_string "backend" with
      | Some "jtw" -> start (Backend.jtw url)
      | _ -> start (Backend.builtin ?extra_load:(current_string "src-load") url)
      )
  | None, None -> error "the script needs a src-worker attribute"
