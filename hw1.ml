(* Roma Razdan *)
(* I pledge my honor that I have abided by the Stevens Honor System *)

type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(* 1. helper for mirror image *)
let mirror_image_helper l = 
    match l with 
    | 3 -> 5
    | 5 -> 3
    | 0 -> 0
    | 1 -> 1
    | 2 -> 4
    | 4 -> 2
    | _ -> -1

(* 1. calls mirror image helper *)
let mirror_image = List.map mirror_image_helper

(* 2. rotate 90 helper *)
let rotate_90_letter_helper l = 
    match l with 
    | 2 -> 3 
    | 3 -> 4
    | 4 -> 5
    | 0 -> 0
    | 1 -> 1
    | 5 -> 2
    | _ -> -1

(* 2. calls rotate 90 helper *)
let rotate_90_letter = List.map rotate_90_letter_helper

(* 3. *)
let rotate_90_word = List.map rotate_90_letter

(* 4. *)
let rec repeat (n: int) (element: 'a) : 'a list =
    match n with 
    | 0 -> [] 
    | x -> element :: repeat (x-1) element

(* same as repeat but accounts for 0 and 1 specifically *)
let rec repeat' (n: int) (element: 'a) : 'a list =
    match n with 
    | 0 -> [] 
    | x ->  if element = 0 || element = 1
            then [element]
            else element :: repeat (x-1) element

(* 5. pantograph helpers *)
let rec pantograph_helper : int list list -> int list = fun l ->
    match l with 
    |[] -> []
    |h::t -> h @ pantograph_helper t

(* 5. calls on pantograph helper *)
let pantograph = fun p l ->
    pantograph_helper(List.map (repeat' p) l)

(* 5. pantograph but without map *)
let rec pantograph_nm  : int -> int list -> int list = fun p l ->
    match l with 
    |[] -> []
    |h::t -> repeat' p h @ pantograph_nm p t 
    
(* 5. pantograph using foldl *)
let pantograph_f : int -> int list -> int list = fun p l ->
    List.fold_left (fun x y -> x @ y) [] (List.map (repeat' p) l)

(* 6. used in coverage *)
let find_coord : int*int -> int -> int*int = fun c d ->
    let (x,y) = c in
    match d with 
    | 0 -> (x,y)
    | 1 -> (x,y)
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1, y)
    | _ -> (x,y)

(* 6. coverage helper  *)
let rec coverage_helper : int*int -> int list -> (int*int) list = fun d l ->
    match l with 
    | [] -> []
    | h::t -> let new_coord = find_coord d h in new_coord :: coverage_helper new_coord t

(* 6. coverage uses helper n find_coord *)
let coverage : int*int -> int list -> (int*int) list = fun p l ->
    p :: coverage_helper p l

(* 7. compress helper function *)
let rec compress_helper : int -> int -> int list -> (int*int) list = fun elem count l ->
    match l with 
    |[] -> [(elem, count)]
    | h::t ->   if h <> elem
                then (elem, count) :: compress_helper h 1 t
                else compress_helper elem (count + 1) t

(* 7. calls on compress_helper *)
let compress : int list -> (int*int) list = fun l ->
    match l with 
    |[] -> []
    | h::t -> compress_helper h 1 t

(* 8. uncompress using repeat *)
let rec uncompress : (int * int) list -> int list = fun l -> 
    match l with 
    | [] -> []
    |  h::t -> let (a,b) = h in repeat b a @ uncompress t

(* 8. uncompress helper for map implementation  using repeat *)
let uncompress_m_helper : (int * int) -> int list = fun t ->
    let (a,b) = t in repeat b a

(* 8. uncompress using map *)
let uncompress_m = fun l ->
    pantograph_helper (List.map (uncompress_m_helper) l)

(* 8. uncompress using foldl *)
let uncompress_f : (int * int) list -> int list = fun l ->
    List.fold_left (fun x y -> x @ y) [] (List.map (uncompress_m_helper) l)

(*  9. optimization helper *)
let rec optimize_helper : int -> int list -> int list = fun state l ->
    match l with
    | [] -> []
    | h::t ->   if h = state 
                then optimize_helper state t 
                else 
                    if h = 0
                    then h :: optimize_helper 0 t
                    else h :: optimize_helper 1 t

(* 9. calls upon helper *)
let optimize : program -> program = fun l ->
    optimize_helper 1 l