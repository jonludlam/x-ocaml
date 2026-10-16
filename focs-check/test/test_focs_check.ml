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

(* Tailcheck *)

let tail name code =
  let r = Tailcheck.check ~name (parse_ok code) in
  print_diags r.loaded.diagnostics;
  List.iter
    (fun (c : Tailcheck.call) ->
      Printf.printf "  not a tail call: %s calls %s, line %d\n" c.caller c.callee
        c.loc.loc_start.pos_lnum)
    r.not_tail;
  r

let () =
  let r = tail "Nsum" "let rec nsum n = if n = 0 then 0 else n + nsum (n - 1)" in
  check "tailcheck: nsum is not tail recursive" (List.length r.not_tail = 1);
  let r = tail "Summing" "let rec summing n t = if n = 0 then t else summing (n - 1) (n + t)" in
  check "tailcheck: summing is" (r.calls <> [] && r.not_tail = [] && r.loaded.ok);
  let r =
    tail "Power"
      {|let rec power x n =
  if n = 0 then 1
  else if n mod 2 = 0 then power (x * x) (n / 2)
  else x * power (x * x) (n / 2)|}
  in
  check "tailcheck: power nests on odd exponents" (List.length r.calls = 2 && List.length r.not_tail = 1);
  let r =
    tail "Try"
      "let rec f n = try if n = 0 then 0 else f (n - 1) with Not_found -> 0"
  in
  check "tailcheck: a call under try is not a tail call" (List.length r.not_tail = 1);
  let r =
    tail "Inner"
      {|let rev xs =
  let rec go acc = function [] -> acc | x :: xs -> go (x :: acc) xs in
  go [] xs
let length xs = let rec go = function [] -> 0 | _ :: xs -> 1 + go xs in go xs|}
  in
  check "tailcheck: inner let rec, one of each"
    (List.length r.calls = 2
    && match r.not_tail with [ c ] -> c.caller = "go" && c.loc.loc_start.pos_lnum = 4 | _ -> false);
  let r =
    tail "Mutual"
      {|let rec even n = if n = 0 then true else odd (n - 1)
and odd n = if n = 0 then false else not (even (n - 1)) && true|}
  in
  check "tailcheck: mutual recursion, odd's call is not" 
    (match r.not_tail with [ c ] -> c.caller = "odd" && c.callee = "even" | _ -> false)

(* Count and Growth *)

let growth ?(input = "fun n -> List.init n Fun.id") ~sizes name code call =
  let l = Count.load ~name (parse_ok code) in
  print_diags l.diagnostics;
  let counts = Count.measure ~name ~input ~call sizes in
  let counts = List.map (fun (n, c) -> (n, Option.value c ~default:(-1))) counts in
  let v = Growth.classify counts in
  Printf.printf "%s%s\n" (Growth.table counts) (Growth.explain v);
  v.best

let doubling = [ 64; 128; 256; 512; 1024; 2048 ]

let () =
  let nrev =
    {|let rec nrev = function [] -> [] | x :: xs -> nrev xs @ [ x ]|}
  in
  check "count: nrev is quadratic"
    (growth ~sizes:doubling "Nrev" nrev "Nrev.nrev" = Growth.Quadratic);
  let rev =
    {|let rev xs = let rec go acc = function [] -> acc | x :: xs -> go (x :: acc) xs in go [] xs|}
  in
  check "count: rev is linear" (growth ~sizes:doubling "Rev" rev "Rev.rev" = Growth.Linear);
  check "count: List.rev counts, and is linear"
    (growth ~sizes:doubling "Librev" "let r l = List.rev l" "Librev.r" = Growth.Linear);
  check "count: nrev through List.append is quadratic"
    (growth ~sizes:doubling "Nrev2"
       "let rec nrev = function [] -> [] | x :: xs -> List.append (nrev xs) [ x ]"
       "Nrev2.nrev"
    = Growth.Quadratic);
  let nsum = "let rec nsum n = if n = 0 then 0 else n + nsum (n - 1)" in
  check "count: nsum is linear"
    (growth ~input:"Fun.id" ~sizes:doubling "Nsum2" nsum "Nsum2.nsum" = Growth.Linear);
  let power =
    {|let rec power x n =
  if n = 0 then 1
  else if n mod 2 = 0 then power (x * x) (n / 2)
  else x * power (x * x) (n / 2)|}
  in
  check "count: power is logarithmic"
    (growth ~input:"Fun.id"
       ~sizes:[ 1000; 2000; 4000; 8000; 16000; 32000; 64000; 128000 ]
       "Power2" power "Power2.power 1"
    = Growth.Log);
  let isort =
    {|let rec insert x = function
  | [] -> [ x ]
  | y :: ys -> if x <= y then x :: y :: ys else y :: insert x ys
let rec isort = function [] -> [] | x :: xs -> insert x (isort xs)|}
  in
  check "count: insertion sort on sorted input is linear"
    (growth ~sizes:doubling "Isort" isort "Isort.isort" = Growth.Linear);
  check "count: insertion sort on reversed input is quadratic"
    (growth ~input:"fun n -> List.rev (List.init n Fun.id)" ~sizes:doubling
       "Isort" isort "Isort.isort"
    = Growth.Quadratic);
  let msort =
    {|let rec merge xs ys =
  match (xs, ys) with
  | [], l | l, [] -> l
  | x :: xs', y :: ys' -> if x <= y then x :: merge xs' ys else y :: merge xs ys'
let rec split = function
  | x :: y :: rest -> let a, b = split rest in (x :: a, y :: b)
  | l -> (l, [])
let rec msort = function
  | ([] | [ _ ]) as l -> l
  | l -> let a, b = split l in merge (msort a) (msort b)|}
  in
  check "count: mergesort is n log n"
    (growth ~input:"fun n -> List.init n (fun i -> (i * 7919) mod n)"
       ~sizes:[ 64; 128; 256; 512; 1024; 2048; 4096 ] "Msort" msort "Msort.msort"
    = Growth.N_log_n);
  let fib = "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)" in
  check "count: naive fib is exponential"
    (growth ~input:"Fun.id" ~sizes:[ 10; 11; 12; 13; 14; 15; 16; 17; 18 ] "Fib" fib "Fib.fib"
    = Growth.Exponential);
  check "count: List.length once is linear, not more"
    (growth ~sizes:doubling "Len" "let f l = List.length l + List.length l" "Len.f"
    = Growth.Linear);
  check "count: a cubic triple loop"
    (growth ~input:"Fun.id" ~sizes:[ 8; 16; 32; 64; 128 ] "Cube"
       "let f n = let c = ref 0 in let rec go i = if i < n * n * n then (incr c; go (i + 1)) in go 0; !c"
       "Cube.f"
    = Growth.Cubic);
  let l = Count.load ~name:"Loop" (parse_ok "let rec loop x = loop x") in
  check "count: fuel stops a run that never ends"
    (l.ok
    && Count.measure ~name:"Loop" ~fuel:100_000 ~input:"Fun.id" ~call:"Loop.loop" [ 1 ]
       = [ (1, None) ])

(* Ban *)

let typed code =
  match Isolated.typecheck (parse_ok code) with
  | Ok t, _ -> t
  | Error (), ds -> print_diags ds; failwith "typecheck"

let violations rules code =
  let vs = Ban.check rules (typed code) in
  List.iter (fun (v : Ban.violation) -> Printf.printf "  %s\n" v.message) vs;
  List.length vs

let () =
  let no_rev = [ Ban.Forbid_value "Stdlib.List.rev" ] in
  check "ban: List.rev" (violations no_rev "let f l = List.rev l" = 1);
  check "ban: List.rev through a module alias" (violations no_rev "module L = List let f l = L.rev l" = 1);
  check "ban: List.rev through a value" (violations no_rev "let r = List.rev let f l = r l" = 1);
  check "ban: List.rev through open" (violations no_rev "open List let f l = rev l" = 1);
  check "ban: the reader's own rev is not List.rev"
    (violations no_rev "let rec rev = function [] -> [] | x :: xs -> rev xs @ [ x ]" = 0);
  check "ban: no @" (violations [ Ban.Forbid_value "Stdlib.@" ] "let f l = l @ l" = 1);
  check "ban: no @, as ( @ )" (violations [ Ban.Forbid_value "Stdlib.@" ] "let app = ( @ )" = 1);
  check "ban: no Array" (violations [ Ban.Forbid_module "Stdlib.Array" ] "let f l = Array.to_list (Array.of_list l)" = 2);
  let loop = "let sum n = let t = ref 0 and i = ref 0 in while !i < n do incr i; t := !t + !i done; !t" in
  check "ban: must use while and no rec (11.2), met"
    (violations [ Ban.Require Ban.While; Ban.Forbid Ban.Let_rec ] loop = 0);
  check "ban: must use while, not met"
    (violations [ Ban.Require Ban.While ] "let rec sum n = if n = 0 then 0 else n + sum (n - 1)" = 1);
  check "ban: must be recursive"
    (violations [ Ban.Require Ban.Recursion ] "let rec f x = x + 1" = 1
    && violations [ Ban.Require Ban.Recursion ] "let rec f x = if x = 0 then 0 else f (x - 1)" = 0);
  let map2 =
    {|let rec map2 f xs ys = match (xs, ys) with
  | x :: xs, y :: ys -> f x y :: map2 f xs ys
  | _ -> []|}
  in
  check "ban: map2 without map, one recursive function (8.3)"
    (violations [ Ban.Forbid_value "Stdlib.List.map" ] map2 = 0
    && Ban.recursive_functions (typed map2) = [ "map2" ]);
  check "ban: mutation" (violations [ Ban.Forbid Ban.Mutation ] loop >= 1);
  (* D8: the library banned, as exercises/harness.ml's uses_library does. *)
  let lib = [ Ban.Forbid_library [] ] in
  let sum = "let rec sum = function [] -> 0 | x :: xs -> x + sum xs\n" in
  check "ban library: wheat, with + @ failwith raise = allowed"
    (violations lib (sum ^ "let isum xs = let rec go acc = function [] -> acc | x :: xs -> go (acc + x) xs in go 0 xs\nlet f l = if l = [] then failwith \"e\" else l @ l\nlet g () = raise Not_found") = 0);
  check "ban library: 3.1 chaff fold-left"
    (violations lib (sum ^ "let isum xs = List.fold_left ( + ) 0 xs") = 1);
  check "ban library: 3.1 chaff stdlib-qualified"
    (violations lib (sum ^ "let isum xs = Stdlib.List.fold_left ( + ) 0 xs") = 1);
  check "ban library: through an alias" (violations lib "module L = Stdlib.List let f = L.length" = 1);
  check "ban library: through open" (violations lib "open List let f = length" = 1);
  check "ban library: Stdlib.failwith, written so" (violations lib "let f () = Stdlib.failwith \"x\"" = 1);
  check "ban library: allowed modules"
    (violations [ Ban.Forbid_library [ "Stdlib.Printf" ] ] "let f = Printf.sprintf \"%d\" 1" = 0)

let () = if !failures > 0 then (Printf.printf "%d failed\n" !failures; exit 1)
