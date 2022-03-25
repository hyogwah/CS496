(*I pledge my honor that I have abided by the Stevens Honor system*)

type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let mirror_helper : int -> int =
    fun l ->
    match l with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 4
    | 3 -> 5
    | 4 -> 2
    | 5 -> 3
    | _ -> failwith "incorrect input"

let mirror_image : int list -> int list =
    fun l ->
    List.map mirror_helper l

let rotate_helper : int -> int =
    fun l ->
    match l with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 3
    | 3 -> 4
    | 4 -> 5
    | 5 -> 2
    | _ -> failwith "incorrect input"

let rotate_90_letter : int list -> int list =
    fun l ->
    List.map rotate_helper l

let rotate_90_word : int list list -> int list list =
    fun l ->
    List.map rotate_90_letter l

let rec repeat : int -> 'a -> 'a list =
    fun a b ->
    match a with
    | 0 -> []
    | _ -> b :: repeat (a - 1) b

let pantograph_helper : int -> int -> int list =
    fun a b ->
    match b with 
    | 0 -> [0]
    | 1 -> [1]
    | _ -> repeat a b

let pantograph : int -> int list -> int list = 
    fun a l ->
    List.concat(List.map (pantograph_helper a) l)

let rec pantograph_nm : int -> int list -> int list = 
    fun a b ->
    match b with
    | [] -> []
    | h::t ->
        if h=0 then [h]@pantograph_nm a t else 
        if h=1 then [h]@pantograph_nm a t else
        (repeat a h) @ (pantograph_nm a t)

let pantograph_f : int -> int list -> int list = 
    fun a l ->
    List.fold_left (fun x y -> x@y) [] (List.map (pantograph_helper a) l)


let coverage_helper : int*int -> int -> int*int =
    fun (x,y) z ->
    match z with
    | 0 -> (x,y)
    | 1 -> (x,y)
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1, y)
    | _ -> failwith "invalid input"

let rec coverage' : int*int -> int list -> (int*int) list =
    fun (x,y) l ->
    match l with
    | [] -> []
    | h::t -> coverage_helper (x,y) h:: coverage' (coverage_helper (x,y) h) t

let coverage : int*int -> int list -> (int*int) list =
    fun (x,y) l ->
    [(x,y)] @ coverage' (x,y) l

let rec compress_helper l c i =
    match l with
    | [] -> [(c,i)]
    | h::t ->
        if h=c then compress_helper t c (i+1) else
        (c,i)::compress_helper t h 1 

let compress : int list -> (int*int) list =
    fun l ->
    compress_helper l (List.hd l) 0

let rec uncompress : (int*int) list -> int list =
    fun l ->
    match l with 
    | [] -> []
    | (x,y)::t ->
        (repeat y x)@uncompress t

let uncompress_helper : (int*int) -> int list =
    fun (x,y) ->
        repeat y x
let uncompress_m : (int*int) list -> int list =
    fun l ->
    List.concat (List.map uncompress_helper l)

let uncompress_f : (int*int) list -> int list =
    fun l ->
    List.fold_left (fun x y -> x@y) [] (List.map uncompress_helper l)

let rec optimize_helper : int -> int list -> int list =
    fun x l ->
    match l with
    | [] -> []
    | h::t -> 
        if h=x then []@optimize_helper x t else
            [h]@optimize_helper h t
let optimize : int list -> int list =
    fun l ->
    optimize_helper 1 l