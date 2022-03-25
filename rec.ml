
(* First example of recursion in OCaml *)

let rec prodl : int list -> int =
    fun l -> 
    match l with
    | [] -> 1
    | h::t -> h * prodl t

let rec prodl' (l:int list) : int =
    match l with
    | [] -> 1
    | h::t -> h * prodl t
    
let rec fact n =
    match n with
    | m when m<0 -> failwith "invalid input"
    | 0 -> 1
    | m -> m * fact (n-1)

let rec mem e l =
    match l with
    | [] -> false
    | h::t -> h=e || mem e t

let rec has_duplicates l =
    match l with
    | [] -> false
    | h::t -> mem h t || has_duplicates t

(* repeat 3 "hello" => ["hello";"hello";"hello"] *)
let rec repeat n e =
    match n with
    | 0 -> []
    | m -> e :: repeat (n - 1) e

let rec repeat' (n:int) (e:'a) : 'a list =
    match n with
    | 0 -> []
    | m -> e :: repeat (n - 1) e

(* recursion on lists *)
let rec sizel l =
    match l with
    | [] -> 0
    | h::t -> 1 + sizel t

let rec suml l =
    match l with
    | [] -> 0
    | h::t -> h + suml t

let rec mem e l =
    match l with
    | [] -> false
    | h::t -> h=e || mem e t
