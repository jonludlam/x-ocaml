type model = Constant | Log | Linear | N_log_n | Quadratic | Cubic | Exponential

let models = [ Constant; Log; Linear; N_log_n; Quadratic; Cubic; Exponential ]

let name = function
  | Constant -> "O(1)"
  | Log -> "O(log n)"
  | Linear -> "O(n)"
  | N_log_n -> "O(n log n)"
  | Quadratic -> "O(n^2)"
  | Cubic -> "O(n^3)"
  | Exponential -> "O(2^n)"

let log2 x = log x /. log 2.

let g model n =
  let n = float n in
  match model with
  | Constant -> 1.
  | Log -> log2 n
  | Linear -> n
  | N_log_n -> n *. log2 n
  | Quadratic -> n *. n
  | Cubic -> n *. n *. n
  | Exponential -> 2. ** n

type fit = { model : model; a : float; b : float; error : float }

type verdict = {
  best : model;
  fits : fit list;
  ratios : (int * float) list;
}

let max_rel_error points f =
  List.fold_left
    (fun acc (n, c) ->
      let c = float c in
      Float.max acc (Float.abs (f n -. c) /. Float.max 1. c))
    0. points

(* c ~ a * x + b, by least squares. *)
let affine xs cs =
  let k = float (List.length xs) in
  let sum = List.fold_left ( +. ) 0. in
  let sx = sum xs and sc = sum cs in
  let sxx = sum (List.map (fun x -> x *. x) xs) in
  let sxc = sum (List.map2 ( *. ) xs cs) in
  let d = (k *. sxx) -. (sx *. sx) in
  if d = 0. then (0., sc /. k)
  else
    let a = ((k *. sxc) -. (sx *. sc)) /. d in
    (a, (sc -. (a *. sx)) /. k)

let fit points model =
  let cs = List.map (fun (_, c) -> float c) points in
  match model with
  | Constant ->
      let b = List.fold_left ( +. ) 0. cs /. float (List.length cs) in
      { model; a = 0.; b; error = max_rel_error points (fun _ -> b) }
  | Exponential ->
      (* log c ~ n log base + log a: any base above one counts. *)
      let xs = List.map (fun (n, _) -> float n) points in
      let ls = List.map (fun c -> log (Float.max 1. c)) cs in
      let slope, icept = affine xs ls in
      let error =
        if slope <= log 1.2 then infinity
        else max_rel_error points (fun n -> exp ((slope *. float n) +. icept))
      in
      { model; a = exp icept; b = exp slope; error }
  | _ ->
      let xs = List.map (fun (n, _) -> g model n) points in
      let a, b = affine xs cs in
      let error =
        if a <= 0. then infinity
        else max_rel_error points (fun n -> (a *. g model n) +. b)
      in
      { model; a; b; error }

let ratios points =
  let rec go = function
    | (_, c0) :: ((n1, c1) :: _ as rest) ->
        (n1, float c1 /. Float.max 1. (float c0)) :: go rest
    | _ -> []
  in
  go points

(* Two fits within this factor of each other are as good: the slower model
   wins, as the simpler explanation. *)
let slack = 1.5
let good_enough = 0.02

let rank m =
  let rec go i = function [] -> i | m' :: ms -> if m = m' then i else go (i + 1) ms in
  go 0 models

let classify points =
  if List.length points < 3 then invalid_arg "Growth.classify: fewer than three sizes";
  let fits = List.map (fit points) models in
  let fits = List.sort (fun f1 f2 -> Float.compare f1.error f2.error) fits in
  let least = (List.hd fits).error in
  let acceptable =
    List.filter (fun f -> f.error <= Float.max good_enough (least *. slack)) fits
  in
  let best =
    List.fold_left
      (fun b f -> if rank f.model < rank b.model then f else b)
      (List.hd acceptable) acceptable
  in
  { best = best.model; fits; ratios = ratios points }

let table points =
  let ratios = ratios points in
  let b = Buffer.create 256 in
  Printf.bprintf b "%10s %14s %10s\n" "n" "steps" "ratio";
  List.iter
    (fun (n, c) ->
      match List.assoc_opt n ratios with
      | None -> Printf.bprintf b "%10d %14d %10s\n" n c ""
      | Some r -> Printf.bprintf b "%10d %14d %10.2f\n" n c r)
    points;
  Buffer.contents b

let explain v =
  match List.rev v.ratios with
  | [] -> name v.best
  | (n1, r) :: rest ->
      let grew =
        match rest with
        | (n0, _) :: _ when n1 = 2 * n0 -> "doubled"
        | (n0, _) :: _ when n1 = n0 + 1 -> "grew by one"
        | (n0, _) :: _ -> Printf.sprintf "grew from %d to %d" n0 n1
        | [] -> "grew"
      in
      Printf.sprintf "when the input %s, it did about %.1f times the work: %s"
        grew r (name v.best)
