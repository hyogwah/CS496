type 'a gt = Node of 'a *( ' a gt ) list

let t : int gt =
  Node (33,
    [Node (12,[]);  Node (77,[
    Node (37,[Node (14,[])]);
        Node (48 , []);Node (103 , [])])
  ])

let t1 : int gt =
  Node (33, [Node (12, [Node (3, [Node (4, []);Node(5,[Node(2,[]);Node(6,[])])])])])
let mk_leaf (n:'a) : 'a gt =
  Node(n, [])

let rec height_helper : int list -> int =
  fun l ->
    match l with
    | [] -> 0
    | h::t -> h + height_helper t

let rec height' : 'a gt -> int =
    fun t ->
    match t with
    | Node( _ , []) -> 0
    | Node( _ , t) -> 1 + height_helper(List.map height' t)

let rec height : 'a gt -> int =
  fun t ->
  1 + height'(t)

let rec size_helper : int list -> int =
  fun l ->
    match l with
    | [] -> 0
    | h::t -> h + size_helper t

let rec size' : 'a gt -> int =
  fun t ->
  match t with
  | Node( _, []) -> 0
  | Node(_, t) -> (List.length t) + size_helper(List.map size' t)

let size : 'a gt -> int =
  fun t ->
  1 + size'(t)


let rec rem_dups : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
      if List.mem h t
      then rem_dups t
      else h:: rem_dups t

let perfect_helper : int list -> bool =
  fun l ->
  if ((List.length (rem_dups l))=0) then true else false

let is_leaf_perfect : 'a gt -> bool =
  fun t ->
    match t with
    | Node(_, l) -> perfect_helper(List.map size l)

let rec path_to_leaves t =
  failwith "implement"
  
let rec preorder_helper : int list list -> int list =
  fun t ->
  match t with
  | [] -> []
  | h::t -> h @preorder_helper t
let rec preorder : 'a gt -> int list =
  fun t ->
  match t with
  | Node (x, []) -> [x]
  | Node (x, l) -> [x]@preorder_helper(List.map preorder l)


let rec mirror : 'a gt -> 'b gt =
  fun t ->
  match t with
  | Node (x, []) -> mk_leaf(x)
  | Node (x, l) -> Node(x, (List.map mirror (List.rev l)))
  
let rec mapt : ( 'a-> 'b) -> 'a gt -> 'b gt =
  fun f t ->
  match t with
  | Node (x, []) -> mk_leaf(f x)
  | Node (d, l) -> Node(f d, List.map (mapt f) l)

let rec sum l = 
  match l with
  | [] -> 0
  | h::t -> h + (sum t)

let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f t ->
  match t with
  | Node (x, []) -> x
  | Node(d,h::t) -> sum (List.map (foldt f) t)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t
  
let mirror' : 'a gt -> 'b gt =
  fun t -> mirror t