(* reverse of a list *)
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

let rec rev l =
failwith "complete"


let rec last l =
failwith "complete"

let rec has_duplicates l =
    match l with
    | [] -> false
    | h::t -> mem h t || has_duplicates t

let rec sublist l1 l2 =
    match l1 with
    | [] -> true
    | h::t -> mem h l2 && sublist t l2


let rec concatenate l1 l2 =
    match l1 with
    | [] -> l2
    | l1-> l1@l2

let rec add_all l =
  match l with
  | [] -> 0
  | (x,y)::t -> x+y+add_all t