(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1: Gregory Baumann
    Name2: Roma Razdan
    
    I pledge my honor that I have abided by the Stevens Honor System

*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33, [Node (12,[]); 
            Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let t2 : int gt =
 Node (33, [Node (12,[]); Node (77, [Node (37, [Node (14, [])]); Node (48, []); Node (103, [])])])

(* 1. returns height or gt *)
let rec height t =
  match t with 
  | Node (d, []) -> 1
  | Node (d, e) -> 1 + List.fold_left (fun x y -> max x y) 0 (List.map height e) 

(* 2. returns size of gt *)    
let rec size t =
  match t with 
  | Node (d, []) -> 1
  | Node (d, e) -> 1 + List.fold_left (fun x y -> x + y) 0 (List.map size e)

(* 3. returns list of path from root to leaves, using mapi *)
let rec paths_to_leaves t =
    match t with
    | Node(d, []) -> [[]]
    | Node(d, e) -> 
    List.concat ( List.mapi (fun i l -> (List.map (fun x -> i::x) l)) (List.map paths_to_leaves e))  

(* 4. returns true if all leaves have the same depth *)
let rec is_perfect_helper l : bool =
    match l with
    | [] -> true
    | [_] -> true
    | h1::h2::t ->
    if (List.length h1) == (List.length h2) then (is_perfect_helper (h2::t))
    else false

(* calls is_perfect_helper *)
let rec is_perfect t = 
  is_perfect_helper (paths_to_leaves t)

(* 5. returns preorder of gt *)
let rec preorder (Node(d,ch)) =
  match ch with
  | [] -> [d]
  | h::t  -> d :: List.fold_left (fun x y -> x @ y) [] (List.map preorder ch)

(* 6. returns mirror of gt *)
let rec mirror (Node(d,ch)) =
  match ch with 
  | [] -> Node(d, [])
  | h::t -> Node(d, List.rev (List.map mirror ch)) 

(* 7. produces gt from t by mapping f to each d *)
let rec mapt f (Node(d,ch)) =
  Node(f d, List.map (mapt f) ch)

(* 8. encodes recusion scheme over gt *)  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
    fun f (Node(d,ch)) -> 
    f d (List.map (foldt f) ch) 
  
let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(* 9. mirrors using foldt *)
let mirror' t  = 
  foldt (fun i rs -> Node(i, List.rev rs)) t
