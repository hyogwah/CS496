type fruit = A | O | K

type fbasket = fruit list

let fb1 = [A;O;O;A;A;K;K;O]

type 'a result = Ok of 'a | Error of string

(* A fruit basket processor (fbp) is any expression whose type is fbasket -> 'a result *)

(* Write a fbp that counts how many oranges there are *)
let no_of_oranges : fbasket -> int result =
  fun fb ->
  Ok(List.length(List.filter (fun i -> i=O) fb))

let no_of_kiwis : fbasket -> int result =
  fun fb ->
  Ok(List.length(List.filter (fun i -> i=K) fb))

let no_of_apples : fbasket -> int result =
  fun fb ->
  Ok(List.length(List.filter (fun i -> i=A) fb))

(* Write a fbp that determines whether there are any apples. This fbp does not return any errors *)
let has_apples : fbasket -> bool result =
  fun fb ->
  Ok (List.mem A fb)

(* Write a fbp that removes an orange, returning the updated fruit basket. 
This fbp returns an error if there are no oranges *)
let rec remove_first_orange =
  fun fb ->
  match fb with
  | [] -> []
  | O::t -> t
  | f::t -> f::remove_first_orange t

let remove_orange : fbasket -> fbasket result =
  fun fb ->
  if (List.mem O fb)
  then Ok(remove_first_orange fb)
  else Error "no orange"
  
(* The constant takes an argument and a fb and ignores the fb entirely, simply returning the argument *)

let const : 'a -> fbasket -> 'a result =
  fun i ->
  fun fb ->
  Ok i

(* Summary of some examples of fbt:
  no_of_oranges : fbasket -> int result
  has_apples    : fbasket -> bool result
  remove_orange : fbasket -> fbasket result
  const 3       : fbasket -> int result
  const "hello" : fbasket -> string result

  They are all instances of type:

  fbasket -> 'a result
*)

(* Write a function sum_fbp 
    Example:/
    sum_fbp no_of_apples no_of_oranges fb1 
    => Ok 6
  
    let fb1 = [A;O;O;A;A;K;K;O]
    *)

let sum_fbp : (fbasket -> int result) -> (fbasket -> int result) -> (fbasket -> int result) =
  fun fb1 fb2 ->
  fun fb ->
  match fb1 fb with 
  | Error s -> Error s
  | Ok n -> 
    (match fb2 fb with
      | Error s -> Error s
      | Ok m -> Ok (n+m))