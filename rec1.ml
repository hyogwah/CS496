(* map *)

let inc i = i+1
let upper c = Char.uppercase_ascii char
let isz i = i=0


let rec succl : int list -> int list =
    fun l ->
    match l with
    | [] -> []
    | h::t -> inc h ::succl t

let rec upperl : char list -> char list =
    fun l ->
    match l with 
    | [] -> []
    | h::t -> upper h::upperl t

let rec is_zerol : int list -> bool list =
    fun l ->
    match l with
    | [] -> []
    | h::t -> isz h ::is_zerol t

let rec map f l =
    match l with
    | [] -> []
    | h::t -> f h :: map f t

let succl' l = map inc l
let upperl' l = map upper l
let is_zerol' l = map isz

(* filter *)

let is_pos i = i>0
let is_uc c = Char.uppercase_ascii c=c
let is_ne l = l<>[]

let rec gtz : int list -> int list =
    fun l ->
    match l with
    | [] -> []
    | h::t ->
        if h>0
        then h:gtz t
        else gtz t

let rec uppercase : char list -> char list =
    fun l ->
    match l with 
    | [] -> []
    | h::t ->
        if Char.uppercase_ascii h =h
        then h::uppercase t
        else uppercase t
    
let rec non_empty : 'a list list -> 'a list list =
    fun l ->
    match l with
    | [] -> []
    | h::t ->
        if h<>[]
        then h::non_empty t
        else non_empty t
