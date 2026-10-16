open Focs_check_core

let failures = ref 0

let check name ok =
  Printf.printf "[%s] %s\n%!" (if ok then "PASS" else "FAIL") name;
  if not ok then incr failures

let () =
  Clflags.debug := false;
  Toploop.initialize_toplevel_env ()

let parse_ok code =
  match Isolated.parse code with
  | Ok str -> str
  | Error ds ->
      List.iter (Format.printf "%a@." Isolated.pp_diagnostic) ds;
      failwith "parse"

let print_diags ds = List.iter (Format.printf "  %a@." Isolated.pp_diagnostic) ds

(* Isolated *)

let () =
  let l = Isolated.load ~name:"Ok1" (parse_ok "let double x = 2 * x") in
  check "isolated: a module loads" (l.ok && l.diagnostics = []);
  let double : int -> int =
    Obj.obj (Isolated.value ~typ:"int -> int" "Ok1.double")
  in
  check "isolated: and its values can be used" (double 21 = 42);
  check "isolated: value checks the type"
    (match Isolated.value ~typ:"string" "Ok1.double" with
    | _ -> false
    | exception Failure _ -> true)

let () =
  let l = Isolated.load ~name:"Bad1" (parse_ok "let x : int = \"no\"") in
  print_diags l.diagnostics;
  check "isolated: a type error is a diagnostic"
    ((not l.ok) && List.length (Isolated.errors l.diagnostics) = 1)

let () =
  let l = Isolated.load ~name:"Raise1" (parse_ok "let x = failwith \"boom\"") in
  print_diags l.diagnostics;
  check "isolated: an exception at load is a diagnostic"
    ((not l.ok) && l.exn <> None)

let () =
  match Isolated.parse ~line:10 "let x = (" with
  | Ok _ -> check "isolated: a syntax error is a diagnostic" false
  | Error ds ->
      print_diags ds;
      check "isolated: a syntax error is a diagnostic, at its line"
        (match ds with [ d ] -> d.loc.loc_start.pos_lnum = 10 | _ -> false)

let () =
  let l =
    Isolated.load ~warnings:"+51" ~name:"Tail1"
      (parse_ok
         "let rec nsum n = if n = 0 then 0 else n + (nsum [@tailcall]) (n - 1)")
  in
  print_diags l.diagnostics;
  check "isolated: warning 51 is kept" (List.length (Isolated.warnings 51 l.diagnostics) = 1)

let () =
  let prelude = Isolated.prelude "let ( @ ) _ _ = [ 42 ]  let unused_in_prelude = 1" in
  let l = Isolated.load ~prelude ~name:"Pre1" (parse_ok "let r = [ 1 ] @ [ 2 ]") in
  let r : int list = Obj.obj (Isolated.value ~typ:"int list" "Pre1.r") in
  check "isolated: the prelude shadows the standard library" (l.ok && r = [ 42 ])

let () =
  match Isolated.typecheck (parse_ok "module L = List let r = L.rev") with
  | Ok _, [] -> check "isolated: typecheck" true
  | _, ds -> print_diags ds; check "isolated: typecheck" false

let () = if !failures > 0 then (Printf.printf "%d failed\n" !failures; exit 1)
