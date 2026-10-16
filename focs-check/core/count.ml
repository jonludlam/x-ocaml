open Parsetree

let tick loc =
  let open Ast_helper in
  Exp.apply ~loc
    (Exp.ident ~loc
       { txt = Longident.Ldot ({ txt = Lident "Focs_count"; loc }, { txt = "tick"; loc }); loc })
    [ (Asttypes.Nolabel, Exp.construct ~loc { txt = Lident "()"; loc } None) ]

let ticking (e : expression) =
  Ast_helper.Exp.sequence ~loc:e.pexp_loc (tick e.pexp_loc) e

let instrument str =
  let open Ast_mapper in
  let expr m e =
    let e = default_mapper.expr m e in
    match e.pexp_desc with
    | Pexp_construct ({ txt = Lident "::"; _ }, Some _) -> ticking e
    | Pexp_function (params, c, Pfunction_body body) when params <> [] ->
        { e with pexp_desc = Pexp_function (params, c, Pfunction_body (ticking body)) }
    | Pexp_function (params, c, Pfunction_cases (cases, loc, attrs)) ->
        let cases = List.map (fun c -> { c with pc_rhs = ticking c.pc_rhs }) cases in
        { e with pexp_desc = Pexp_function (params, c, Pfunction_cases (cases, loc, attrs)) }
    | _ -> e
  in
  let m = { default_mapper with expr } in
  m.structure m str

(* The counting standard library: a step for each element gone through or
   built, as the reader's own version of each would take. *)
let prelude_code =
  {|
module Focs_count = struct
  exception Out_of_fuel
  let steps = ref 0
  let fuel = ref max_int
  let tick () = incr steps; if !steps > !fuel then raise Out_of_fuel
  let add n = steps := !steps + n; if !steps > !fuel then raise Out_of_fuel
end

let ( @ ) xs ys =
  let rec app = function
    | [] -> ys
    | x :: xs -> Focs_count.tick (); x :: app xs
  in
  app xs

module List = struct
  include Stdlib.List
  let tick = Focs_count.tick
  let length l = Focs_count.add (Stdlib.List.length l); Stdlib.List.length l
  let compare_lengths a b =
    let r = Stdlib.List.compare_lengths a b in
    Focs_count.add (min (Stdlib.List.length a) (Stdlib.List.length b)); r
  let rev_append xs ys =
    let rec go xs acc = match xs with [] -> acc | x :: xs -> tick (); go xs (x :: acc) in
    go xs ys
  let rev xs = rev_append xs []
  let append = ( @ )
  let concat ls = Stdlib.List.fold_right ( @ ) ls []
  let flatten = concat
  let nth l n = Focs_count.add (max 0 n); Stdlib.List.nth l n
  let nth_opt l n = Focs_count.add (max 0 n); Stdlib.List.nth_opt l n
  let init n f = Focs_count.add (max 0 n); Stdlib.List.init n f
  let map f l = Stdlib.List.map (fun x -> tick (); f x) l
  let mapi f l = Stdlib.List.mapi (fun i x -> tick (); f i x) l
  let rev_map f l = Stdlib.List.rev_map (fun x -> tick (); f x) l
  let filter_map f l = Stdlib.List.filter_map (fun x -> tick (); f x) l
  let concat_map f l = concat (map f l)
  let iter f l = Stdlib.List.iter (fun x -> tick (); f x) l
  let iteri f l = Stdlib.List.iteri (fun i x -> tick (); f i x) l
  let fold_left f a l = Stdlib.List.fold_left (fun a x -> tick (); f a x) a l
  let fold_right f l a = Stdlib.List.fold_right (fun x a -> tick (); f x a) l a
  let map2 f a b = Stdlib.List.map2 (fun x y -> tick (); f x y) a b
  let iter2 f a b = Stdlib.List.iter2 (fun x y -> tick (); f x y) a b
  let fold_left2 f a l1 l2 = Stdlib.List.fold_left2 (fun a x y -> tick (); f a x y) a l1 l2
  let fold_right2 f l1 l2 a = Stdlib.List.fold_right2 (fun x y a -> tick (); f x y a) l1 l2 a
  let for_all p l = Stdlib.List.for_all (fun x -> tick (); p x) l
  let exists p l = Stdlib.List.exists (fun x -> tick (); p x) l
  let for_all2 p a b = Stdlib.List.for_all2 (fun x y -> tick (); p x y) a b
  let exists2 p a b = Stdlib.List.exists2 (fun x y -> tick (); p x y) a b
  let mem x l = Stdlib.List.exists (fun y -> tick (); y = x) l
  let memq x l = Stdlib.List.exists (fun y -> tick (); y == x) l
  let find p l = Stdlib.List.find (fun x -> tick (); p x) l
  let find_opt p l = Stdlib.List.find_opt (fun x -> tick (); p x) l
  let find_map f l = Stdlib.List.find_map (fun x -> tick (); f x) l
  let filter p l = Stdlib.List.filter (fun x -> tick (); p x) l
  let find_all = filter
  let filteri p l = Stdlib.List.filteri (fun i x -> tick (); p i x) l
  let partition p l = Stdlib.List.partition (fun x -> tick (); p x) l
  let assoc k l = snd (find (fun (k', _) -> k' = k) l)
  let assoc_opt k l = Option.map snd (find_opt (fun (k', _) -> k' = k) l)
  let mem_assoc k l = exists (fun (k', _) -> k' = k) l
  let remove_assoc k l = filter (fun (k', _) -> k' <> k) l
  let split l = Focs_count.add (Stdlib.List.length l); Stdlib.List.split l
  let combine a b = Focs_count.add (Stdlib.List.length a); Stdlib.List.combine a b
  let sort cmp l = Stdlib.List.sort (fun a b -> tick (); cmp a b) l
  let stable_sort cmp l = Stdlib.List.stable_sort (fun a b -> tick (); cmp a b) l
  let sort_uniq cmp l = Stdlib.List.sort_uniq (fun a b -> tick (); cmp a b) l
  let merge cmp a b = Stdlib.List.merge (fun a b -> tick (); cmp a b) a b
  let equal eq a b = Stdlib.List.equal (fun x y -> tick (); eq x y) a b
  let compare cmp a b = Stdlib.List.compare (fun x y -> tick (); cmp x y) a b
  let to_seq l = Focs_count.add (Stdlib.List.length l); Stdlib.List.to_seq l
  let of_seq s = let l = Stdlib.List.of_seq s in Focs_count.add (Stdlib.List.length l); l
end
|}

let prelude = Isolated.prelude prelude_code

let load ~name str = Isolated.load ~prelude ~name (instrument str)

let measure ~name ?(fuel = 10_000_000) ~input ~call sizes =
  let run : int -> int -> int =
    Obj.obj
      (Isolated.value ~typ:"int -> int -> int"
         (Printf.sprintf
            "fun fuel n ->\n\
            \  let x = (%s) n in\n\
            \  %s.Focs_count.steps := 0;\n\
            \  %s.Focs_count.fuel := fuel;\n\
            \  match ignore ((%s) x) with\n\
            \  | () -> %s.Focs_count.fuel := max_int; !%s.Focs_count.steps\n\
            \  | exception %s.Focs_count.Out_of_fuel -> %s.Focs_count.fuel := max_int; -1"
            input name name call name name name name))
  in
  List.map
    (fun n ->
      let c = run fuel n in
      (n, if c < 0 then None else Some c))
    sizes
