(* Roma Razdan and Greg Baumann *)

(* Quiz 2 *)
(* 
   One submission per group
   Name of both partners in source code
   Name of other member (the one not submitting) as a canvas comment
*)

(* [prune n t] prunes the tree [t] at level [n].
   If [n] is larger than the height of [t], then it returns [t].
   Also, [prune 0 t] ==> Empty 
*)

let rec paths_to_leaves t = 
    match t with 
    | Empty -> []
    | Node (d, Empty, Empty) -> [ [] ]
    | Node (d, lt, rt) ->  List.map(fun l -> 0::1) (paths_to_leaves rt) @ List.map(fun l -> 1::1) (paths_to_leaves lt)
 

let rec prune n t =
  match n with
  | 0 -> Empty
  | _ -> 
  match t with
  | Empty -> Empty
    | Node (d,lt,rt) -> 
    if height t < n then Empty
    else Node(d,prune (n-1) lt,prune (n-1) rt)


let rec height t = 
    match t with 
    | Empty -> 0 
    | Node (_, lt, rt) -> 1 + max (height lt) (height rt)



(* [perfect t] determines whether [t] is a perfect binary tree
   [t] is perfect if the length of all the
 * paths to all the leaves is the same *)
(*let perfect t =
    match t with 
    | Empty -> []
    | Node (d, lt, rt)  -> List.map(fun l -> 0::1) (paths_to_leaves rt, paths_to_leaves lt) @ List.map(fun l -> 1:1) (heigt t) 
  *)
  let perfect t =
  match t with
  Empty -> Empty
  Node(d,lt,rt) ->
  if perfect rt == perfect lt 
    then true
    else false