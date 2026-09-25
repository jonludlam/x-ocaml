open Brr

let key id =
  Jstr.of_string
    ("xo:" ^ Jstr.to_string (Uri.path (Window.location G.window)) ^ ":" ^ id)

(* Reading localStorage throws where the browser refuses it. *)
let storage () =
  try Some (Brr_io.Storage.local G.window) with Jv.Error _ -> None

let find id =
  Option.bind (storage ()) (fun s ->
      Option.map Jstr.to_string (Brr_io.Storage.get_item s (key id)))

let keep id source =
  Option.iter
    (fun s ->
      ignore (Brr_io.Storage.set_item s (key id) (Jstr.of_string source)))
    (storage ())

let forget id =
  Option.iter (fun s -> Brr_io.Storage.remove_item s (key id)) (storage ())
