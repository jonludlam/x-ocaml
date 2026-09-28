(* The page's half of widgets, as displays:

   <x-ocaml-view>, data {"id", "token", "view", "command"?}: a widget's view,
   patched in place at each update; its events go back through the callback
   token. A command, numbered, is for the managed widget the view is.

   <x-ocaml-widget-adapter>, data {"kind", "js"}: an adapter for managed
   widgets; it shows nothing. *)

open Brr

type mounted = {
  ctx : Widget_view.ctx;
  mutable view : Widget_view.rendered option;
  mutable commands : int; (* the last command carried out *)
}

let mounted : (El.t * mounted) list ref = ref []

let mount el ~id ~token =
  match List.assq_opt el !mounted with
  | Some m -> m
  | None ->
      El.set_class (Jstr.v "x-ocaml-widget") true el;
      El.set_at (Jstr.v "data-widget-id") (Some (Jstr.v id)) el;
      let send ~widget_id:_ ~handler_id ~event_type:_ ~value =
        let value = match value with Some v -> Jv.of_string v | None -> Jv.null in
        Element.callback el ~token
          (Jstr.to_string
             (Json.encode (Jv.obj [| ("handler", Jv.of_string handler_id); ("value", value) |])))
      in
      let m = { ctx = { widget_id = id; send; adapters = Widget_view.adapters }; view = None; commands = 0 } in
      mounted := (el, m) :: !mounted;
      m

let on_view el data =
  match Json.decode (Jstr.v data) with
  | Error _ -> ()
  | Ok d -> (
      let m = mount el ~id:(Jv.to_string (Jv.get d "id")) ~token:(Jv.to_int (Jv.get d "token")) in
      (match Widget_view.decode (Jv.get d "view") with
       | Error msg ->
           Option.iter (Widget_view.drop m.ctx) m.view;
           m.view <- None;
           El.set_children el [ Widget_view.placeholder ("malformed widget view: " ^ msg) ]
       | Ok n -> (
           match m.view with
           | Some r -> m.view <- Some (Widget_view.patch m.ctx r n)
           | None ->
               let r = Widget_view.render m.ctx n in
               El.set_children el [ Widget_view.dom r ];
               m.view <- Some r));
      match Jv.find d "command" with
      | Some c when Jv.to_int (Jv.get c "n") > m.commands ->
          m.commands <- Jv.to_int (Jv.get c "n");
          Option.iter
            (fun r -> Widget_view.command m.ctx r (Jv.to_string (Jv.get c "name")) (Jv.to_string (Jv.get c "data")))
            m.view
      | _ -> ())

let on_gone el =
  match List.assq_opt el !mounted with
  | Some m ->
      Option.iter (Widget_view.drop m.ctx) m.view;
      mounted := List.filter (fun (e, _) -> e != el) !mounted
  | None -> ()

let on_adapter el data =
  El.set_inline_style (Jstr.v "display") (Jstr.v "none") el;
  match Json.decode (Jstr.v data) with
  | Error _ -> ()
  | Ok d ->
      let kind = Jv.to_string (Jv.get d "kind") in
      Widget_view.register_adapter kind (Jv.to_string (Jv.get d "js"));
      (* Managed widgets shown before their adapter came start now. *)
      List.iter
        (fun (_, m) ->
          Option.iter
            (fun r -> List.iter (Widget_view.start m.ctx) (Widget_view.waiting kind r))
            m.view)
        !mounted

let () =
  Element.define "x-ocaml-view" ~on_data:on_view ~on_disconnect:on_gone;
  Element.define "x-ocaml-widget-adapter" ~on_data:on_adapter ~on_disconnect:ignore
