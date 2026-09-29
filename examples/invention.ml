(* Bach's Invention No. 1 in C (BWV 772), as a program.

   Pitches are diatonic: a step is a letter name and an octave, counted
   up from C0, so that moving a figure by steps keeps it in the scale, as
   Bach's sequences do. Sharps and flats come from the key, which changes
   as the piece modulates, or from a patch on a single note. *)

(* {1 Notes and lines} *)

type ornament = Mordent | Prall

(* Where a note came from, for colouring the score. *)
type origin = A | A_inv | B | Counter | Counter_inv | Thirds | Held | Free

type note = {
  steps : int list; (* [] is a rest, several notes a chord; C4 is 28 *)
  alt : int option; (* semitones up; None: as the key says *)
  dur : int; (* in thirty-seconds *)
  orn : ornament option;
  origin : origin;
}

type line = note list

let thirty_second = 1
let sixteenth = 2
let eighth = 4
let quarter = 8
let half = 16
let whole = 32
let length l = List.fold_left (fun n x -> n + x.dur) 0 l

let letter_index c =
  match c with
  | 'c' -> 0 | 'd' -> 1 | 'e' -> 2 | 'f' -> 3 | 'g' -> 4 | 'a' -> 5 | 'b' -> 6
  | _ -> invalid_arg (Printf.sprintf "not a note name: %c" c)

(* [p "g4"] is the step of G above middle C. *)
let p s =
  if String.length s <> 2 then invalid_arg ("p: " ^ s);
  (7 * (Char.code s.[1] - Char.code '0')) + letter_index s.[0]

let note ?(origin = Free) dur step =
  { steps = [ step ]; alt = None; dur; orn = None; origin }

let rest dur = { steps = []; alt = None; dur; orn = None; origin = Free }

let chord dur names =
  { steps = List.map p names; alt = None; dur; orn = None; origin = Free }

(* {2 Literal music}

   [notes "8 c4 b3 f#3 16 d5 8. c5!p"]: a number sets the duration, as in
   Humdrum (4 a quarter, 16 a sixteenth, a dot adds half, [2+16] a half
   tied to a sixteenth), and lasts until the next; [r] is a rest; a
   pitch may be sharp, flat or natural ([#], [b], [n]) and carry an
   ornament ([!m] a mordent, [!p] a prall). *)
let notes s =
  let dur_of tok =
    let one t =
      let dotted = String.ends_with ~suffix:"." t in
      let t = if dotted then String.sub t 0 (String.length t - 1) else t in
      let d = 32 / int_of_string t in
      if dotted then d + (d / 2) else d
    in
    List.fold_left (fun n t -> n + one t) 0 (String.split_on_char '+' tok)
  in
  let pitch tok =
    let tok, orn =
      match String.index_opt tok '!' with
      | None -> (tok, None)
      | Some i ->
          ( String.sub tok 0 i,
            match String.sub tok (i + 1) (String.length tok - i - 1) with
            | "m" -> Some Mordent
            | "p" -> Some Prall
            | o -> invalid_arg ("notes: ornament " ^ o) )
    in
    let alt, name =
      match String.length tok with
      | 2 -> (None, tok)
      | 3 ->
          ( Some (match tok.[1] with '#' -> 1 | 'b' -> -1 | 'n' -> 0
                  | _ -> invalid_arg ("notes: " ^ tok)),
            String.make 1 tok.[0] ^ String.make 1 tok.[2] )
      | _ -> invalid_arg ("notes: " ^ tok)
    in
    (p name, alt, orn)
  in
  let dur = ref quarter in
  String.split_on_char ' ' s
  |> List.filter (fun t -> t <> "" && t <> "|")
  |> List.filter_map (fun tok ->
         if tok.[0] >= '0' && tok.[0] <= '9' then (dur := dur_of tok; None)
         else if tok = "r" then Some (rest !dur)
         else
           let step, alt, orn = pitch tok in
           Some { (note !dur step) with alt; orn })

(* {1 Transformations} *)

let ( ++ ) = ( @ )
let map_steps f l = List.map (fun x -> { x with steps = List.map f x.steps; alt = None }) l

(* Up [k] steps of the scale. *)
let transpose k l = map_steps (fun s -> s + k) l

let first_step l =
  match List.find_opt (fun x -> x.steps <> []) l with
  | Some { steps = s :: _; _ } -> s
  | _ -> invalid_arg "invert: a line of rests"

let inverse = function
  | A -> A_inv | A_inv -> A | Counter -> Counter_inv | Counter_inv -> Counter | o -> o

(* Upside down, about its first note. *)
let invert l =
  let axis = first_step l in
  map_steps (fun s -> (2 * axis) - s) l
  |> List.map (fun x -> { x with origin = inverse x.origin })

let take n l = List.filteri (fun i _ -> i < n) l
let drop n l = List.filteri (fun i _ -> i >= n) l
let tagged origin l = List.map (fun x -> { x with origin }) l

(* {2 Patches} on the [i]th note of a line, counting from 0. *)
let patch i f l = List.mapi (fun j x -> if i = j then f x else x) l
let sharp i = patch i (fun x -> { x with alt = Some 1 })
let natural i = patch i (fun x -> { x with alt = Some 0 })
let ornament i o = patch i (fun x -> { x with orn = Some o })
let replace i step = patch i (fun x -> { x with steps = [ step ]; alt = None })

(* {1 The motives}

   Nearly everything grows from two figures, heard at once in bar 1:
   [a], a run of sixteenths up four notes and back, and [b], the
   eighths that follow, leaping up and turning. Both are written as
   steps above the note the run starts on. *)

let shape origin dur offsets start =
  List.map (fun k -> note ~origin dur (start + k)) offsets

let a = shape A sixteenth [ 0; 1; 2; 3; 1; 2; 0 ]
let a_inv start = invert (a start)
let b = shape B eighth [ 4; 7; 6; 7 ]

(* Each run of [a] starts on the second sixteenth of its half bar; the
   first is a rest, or a note joining it to what came before. *)
let pickup step = [ note ~origin:A sixteenth step ]
let r16 = [ rest sixteenth ]

(* The subject: the run and the eighths, one bar. *)
let subject start = r16 ++ a start ++ b start

(* A note held over the next half bar. *)
let held step = [ note ~origin:Held (half + sixteenth) step ]

(* The inverted run, falling in thirds, each run picked up from the
   step above; the first comes from further off. *)
let descent start n =
  List.init n (fun i ->
      let s = start - (2 * i) in
      pickup (if i = 0 then s - 3 else s + 1) ++ a_inv s)
  |> List.concat

(* A falling line of thirds: the end of the inverted run, repeated. *)
let thirds start = shape Thirds sixteenth [ 0; 2; 1; 3 ] start

(* A voice's first note of a bar, then silence until the run. *)
let echo step run = [ note eighth step; rest eighth; rest quarter ] ++ r16 ++ run

(* The left hand's eighths under the first descent. *)
let counter =
  tagged Counter (notes "8 c4 b3 c4 d4 e4 g3 a3 b3 | c4 e3 f3 g3 a3 b3 4+16 c4")

(* {1 Keys}

   A key says how each letter name is altered. The piece visits G major,
   D minor and A minor, and comes home by way of F. *)

type key = { name : string; alts : (char * int) list }

let key name alts = { name; alts }
let c_major = key "C major" []
let g_major = key "G major" [ ('f', 1) ]
let f_major = key "F major" [ ('b', -1) ]
let d_minor = key "D minor" [ ('b', -1); ('c', 1) ]
let d_minor_rising = key "D minor, rising" [ ('c', 1) ]
let a_minor = key "A minor" [ ('g', 1) ]
let a_minor_rising = key "A minor, rising" [ ('f', 1); ('g', 1) ]
let letters = "cdefgab"

let alt_in key step =
  match List.assoc_opt letters.[step mod 7] key.alts with Some a -> a | None -> 0

(* {1 The score}

   A score is a list of passages, each the two hands over the same
   stretch of music (though a note held over a join may belong to either
   side of it), and the keys each hand is in from which bar (and
   sixteenth) on. *)

type score = {
  passages : (string * line * line) list; (* name, right hand, left hand *)
  right_keys : ((int * int) * key) list;
  left_keys : ((int * int) * key) list;
}

let bars_1_2 =
  ( subject (p "c4") |> ornament 10 Mordent,
    pickup (p "d5") ++ a (p "g4") ++ b (p "g4") |> ornament 10 Mordent )

(* Bars 3 to 5: the inverted run falls in the right hand over the
   eighths of the left, to a half close in G. *)
let bars_3_5 =
  ( descent (p "a5") 4 ++ notes "8 a4 d4 8. c5!p 16 d5",
    counter ++ a (p "d3") )

let invention =
  let u35, l35 = bars_3_5 in
  let passages =
    [
      ("exposition",
        fst bars_1_2 ++ snd bars_1_2,
        [ rest half ] ++ r16 ++ a (p "c3")
        ++ notes "8 g3 g2 4 r" ++ r16 ++ a (p "g3"));
      ("first descent", u35, l35);
      ("to G",
        pickup (p "b4") ++ a_inv (p "a4") ++ thirds (p "g4") ++ thirds (p "b4")
        ++ notes "16 d5 32 b4 c5 16 d5 g5 8 b4!m 16 a4 g4",
        notes "8 g3 b2 c3 d3 | e3 f3 g3 e3 8. b2 16 c3 8 d3 d2");
      ("exposition in G, hands swapped",
        echo (p "g4") (a (p "g4")) ++ (echo (p "f4") (a (p "a4")) |> ornament 0 Mordent),
        (* the right hand's first two bars, a twelfth lower *)
        transpose (-10) (fst bars_1_2 ++ snd bars_1_2)
        |> List.map (fun x -> { x with orn = None }));
      ("inverted",
        echo (p "b4") (a_inv (p "d5")) ++ echo (p "c5") (a_inv (p "e5") |> sharp 5),
        (let bar_9 = pickup (p "g3") ++ a_inv (p "g4") ++ notes "8 f4 e4 f4 d4" in
         bar_9 ++ (transpose 1 bar_9 |> replace 0 (p "e4"))));
      ("the descent again, hands swapped",
        (* bars 3 to 5, the left hand's part an octave and a step higher *)
        transpose 8 l35 ++ notes "16 e5 d5 c5 e5 d5 c5 b4 d5",
        transpose (-6) u35 ++ (pickup (p "c4") ++ a_inv (p "b3") |> natural 3));
      ("to A minor",
        notes "16 c5 a5 g5 b5 a5 e5 f5 d5 g4 f5 e5 d5 8 c5 16 b4 a4",
        thirds (p "a3") ++ thirds (p "c4") ++ notes "8 e4 a3 e4 e3");
      ("sequence over held notes",
        (let two_bars k = transpose k (held (p "g5") ++ a (p "e5") ++ held (p "f5")) in
         pickup (p "a4") ++ a_inv (p "a5")
         ++ two_bars 0 ++ a_inv (p "g5")
         ++ two_bars (-1) ++ a (p "c5")),
        (let two_bars k = transpose k (a_inv (p "e4") ++ held (p "d4") ++ a (p "a3")) in
         notes "8 a3 a2 4 r" ++ r16
         ++ (two_bars 0 |> sharp 5) ++ held (p "b3")
         ++ two_bars (-1)));
      ("home",
        pickup (p "d5") ++ a (p "e5") ++ pickup (p "f5") ++ a (p "g5")
        ++ notes "8 c6 g5 e5 16 d5 c5"
        ++ pickup (p "c5") ++ a_inv (p "b4")
        ++ notes "16 a4 bn4 c5 e4 d4 c5 f4 b4" ++ [ chord whole [ "e4"; "g4"; "c5" ] ],
        (* the eighths of bars 3 and 4, upside down *)
        transpose (-2) (invert (take 12 counter))
        ++ pickup (p "e4") ++ a (p "d3")
        ++ notes "8 e3 c3 d3 e3 16 f3 d3 e3 f3 8 g3 g2"
        ++ [ chord whole [ "c2"; "c3" ] ]);
    ]
  in
  {
    passages;
    right_keys =
      [ ((1, 0), c_major); ((4, 0), g_major); ((9, 0), c_major);
        ((11, 0), d_minor_rising); ((12, 0), a_minor_rising); ((14, 0), a_minor);
        ((15, 0), c_major); ((21, 0), f_major); ((21, 8), c_major) ];
    left_keys =
      [ ((1, 0), c_major); ((3, 0), g_major); ((9, 0), c_major);
        ((11, 0), d_minor); ((12, 0), a_minor); ((13, 0), a_minor_rising);
        ((14, 0), a_minor); ((15, 0), c_major); ((18, 0), f_major); ((20, 0), c_major) ];
  }

(* {1 Performing the score}

   Each hand becomes a list of events: when, which pitches (as MIDI
   numbers), for how long, with the ornament and origin of the note. *)

type event = {
  at : int; (* thirty-seconds from the start *)
  midi : int list;
  len : int;
  ornament : ornament option;
  from : origin;
}

let semitones = [| 0; 2; 4; 5; 7; 9; 11 |]
let midi_of step alt = (12 * ((step / 7) + 1)) + semitones.(step mod 7) + alt

let key_at keys at =
  List.fold_left
    (fun k ((bar, six), key) -> if ((bar - 1) * whole) + (six * sixteenth) <= at then key else k)
    c_major keys

let perform keys line =
  let at = ref 0 in
  List.map
    (fun x ->
      let key = key_at keys !at in
      let e =
        {
          at = !at;
          midi =
            List.map
              (fun s -> midi_of s (match x.alt with Some a -> a | None -> alt_in key s))
              x.steps;
          len = x.dur;
          ornament = x.orn;
          from = x.origin;
        }
      in
      at := !at + x.dur;
      e)
    line

let hands score =
  let right = List.concat_map (fun (_, r, _) -> r) score.passages in
  let left = List.concat_map (fun (_, _, l) -> l) score.passages in
  if length right <> length left then
    failwith
      (Printf.sprintf "the right hand lasts %d thirty-seconds, the left %d"
         (length right) (length left));
  (perform score.right_keys right, perform score.left_keys left)

(* {1 Writing the score out}

   As ABC notation, which abcjs draws and plays. Notes held over a bar
   line are split and tied; sharps and flats are written as the key
   signature (none) and earlier accidentals in the bar require. Each
   note's place in the text is kept with its origin, to colour it. *)

let abc_pitch step =
  let letter = String.make 1 letters.[step mod 7] and oct = step / 7 in
  if oct >= 5 then letter ^ String.make (oct - 5) '\''
  else String.uppercase_ascii letter ^ String.make (4 - oct) ','

(* Lengths in sixteenths, which is the unit (L:1/16). *)
let abc_length d =
  match d with 1 -> "/2" | 2 -> "" | 3 -> "3/2" | d -> string_of_int (d / 2)

let writable d = List.mem d [ 1; 2; 3; 4; 6; 8; 12; 16; 24; 32 ]

(* An event in pieces: none crosses a bar line, each has a written length. *)
let pieces e =
  let rec cut at len =
    if len = 0 then []
    else
      let to_bar = whole - (at mod whole) in
      let n = if len > to_bar then to_bar else len in
      let n =
        if writable n then n else quarter - (at mod quarter) (* up to the beat *)
      in
      (at, n) :: cut (at + n) (len - n)
  in
  cut e.at e.len

let abc_voice ~bars_per_line events =
  (* Each piece's text, the bar it is in, and the origin of its note. *)
  let accidentals = Hashtbl.create 8 and bar = ref (-1) in
  let pieces =
    List.concat_map
      (fun e ->
        let ps = pieces e in
        List.mapi
          (fun i (at, len) ->
            if at / whole <> !bar then (bar := at / whole; Hashtbl.reset accidentals);
            let tied = i < List.length ps - 1 in
            let pitch midi =
              (* recover the step and alteration from the MIDI number *)
              let pc = midi mod 12 and oct = (midi / 12) - 1 in
              let letter, alt =
                match pc with
                | 0 -> (0, 0) | 1 -> (0, 1) | 2 -> (1, 0) | 3 -> (2, -1) | 4 -> (2, 0)
                | 5 -> (3, 0) | 6 -> (3, 1) | 7 -> (4, 0) | 8 -> (4, 1) | 9 -> (5, 0)
                | 10 -> (6, -1) | _ -> (6, 0)
              in
              let step = (7 * oct) + letter in
              let now = Option.value ~default:0 (Hashtbl.find_opt accidentals step) in
              Hashtbl.replace accidentals step alt;
              (if alt = now then "" else match alt with 1 -> "^" | -1 -> "_" | _ -> "=")
              ^ abc_pitch step
            in
            let body =
              match e.midi with
              | [] -> "z"
              | [ m ] -> pitch m
              | ms -> "[" ^ String.concat "" (List.map pitch ms) ^ "]"
            in
            let deco =
              match (i, e.ornament) with
              | 0, Some Mordent -> "!mordent!"
              | 0, Some Prall -> "!pralltriller!"
              | _ -> ""
            in
            let space = if at mod quarter = 0 && at mod whole <> 0 then " " else "" in
            ( at / whole,
              space,
              deco ^ body ^ abc_length len ^ (if tied then "-" else ""),
              e.midi <> [],
              e.from ))
          ps)
      events
  in
  let bars = List.fold_left (fun n (b, _, _, _, _) -> max n (b + 1)) 0 pieces in
  (bars, pieces, bars_per_line)

let abc ?(bars_per_line = 3) ?(title = "Invention No. 1") score =
  let right, left = hands score in
  let buf = Buffer.create 4096 and marks = ref [] in
  Buffer.add_string buf
    (Printf.sprintf
       "X:1\nT:%s\nC:J. S. Bach, BWV 772\nM:4/4\nL:1/16\nQ:1/4=66\n%%%%staves {1 2}\nK:C\nV:1 clef=treble\nV:2 clef=bass\n"
       title);
  let voices = List.map (abc_voice ~bars_per_line) [ right; left ] in
  let bars = List.fold_left (fun n (b, _, _) -> max n b) 0 voices in
  let line = ref 0 in
  while !line * bars_per_line < bars do
    let first = !line * bars_per_line in
    let last = min bars (first + bars_per_line) - 1 in
    List.iteri
      (fun v (_, pieces, _) ->
        Buffer.add_string buf (Printf.sprintf "[V:%d] " (v + 1));
        for b = first to last do
          List.iter
            (fun (pb, space, text, sounding, from) ->
              if pb = b then begin
                Buffer.add_string buf space;
                if sounding then marks := (Buffer.length buf, from) :: !marks;
                Buffer.add_string buf text
              end)
            pieces;
          Buffer.add_string buf (if b = bars - 1 then " |]" else " | ")
        done;
        Buffer.add_char buf '\n')
      voices;
    incr line
  done;
  (Buffer.contents buf, List.rev !marks)
