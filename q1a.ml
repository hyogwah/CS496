(* Eric Song & Chang-Woo Ham
I pledge my honor that I have abided by the Stevens Honor system *)

(* Sample Directed Graph *)


let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]



(*

  1 <------ 3

  |       / |

  |     /   |

  |    /    |

 \/ |/_    \/

  2        4

*)



(*

Eg. outgoing ex 3 => [1,4]
Assume n is integer > 0
*)

let rec mem e l =
    match l with
    | [] -> false
    | h::t -> h=e || mem e t

let appender x l =
   match x with
   | x -> x::l

let same x y =
   if x==y then true else false

let rec has_duplicates l =
    match l with
    | [] -> false
    | h::t -> mem h t || has_duplicates t

let rec outgoing_nodes g n =
   match g with
   | [] -> []
   | (x,y)::t -> if x=n then y::outgoing_nodes t n else outgoing_nodes t n

let rec outgoing_nodes' g n =
   match g with
   | [] -> []
   | h::t -> if h=n then snd h::outgoing_nodes' t n else outgoing_nodes' t n


(*

   The list of nodes of the tree without duplicates. The order of the

   nodes in the list is irrelevant.

   eg. nodes ex => [1,2,3,4]

*)

let rec rem_dups l =
   match l with
   | [] -> []
   | h::t ->
      if List.mem h t
      then rem_dups t
      else h:: rem_dups t

let rec nodes' g =
   match g with
   | [] -> []
   | (x,y)::t -> x::y::nodes t

let rec nodes g =
   rem_dups (nodes' g)
(*

   Remove a node from the graph

   Eg. remove ex 2 =>  [(3, 1); (3, 4)]

*)

let rec remove g n =

   failwith "implement me"



(* Reachable nodes from a source node. (Extra-credit)

   Eg. reachale ex 3 => [1,4,2,3]

   *)


let rec reachable g n =

  failwith "implement me"
