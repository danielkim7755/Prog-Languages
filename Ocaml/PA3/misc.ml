(*  CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 *
 * This function takes in an integer list
 * and returns the sum of the squares of all
 * the elements within that list.
 *)

let sqsum xs = 
  let f a x = a + (x*x) in (* Add x^2 to the accumulator *)
  let base = 0 in          (* Start with 0*)
    List.fold_left f base xs


(* pipe : ('a -> 'a) list -> ('a -> 'a)
 *
 * This function takes in a list of functions and returns
 * the final value after all the functions have been ran on it.
 *)

let pipe fs = 
  let f a x = fun f'-> x (a f') in (* Return a function that outputs fn(..(f2(f1 x)))*)
  let base = fun x -> x in         (* Start with a function that returns itself *)
    List.fold_left f base fs

(* stepConcat : string -> string list -> string
 *
 * This function takes 
 *)

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in      (* Append the accumulator with sep and element *)
      let base = h in                 (* Set Base as the head of the string list *)
      let l = t in
        List.fold_left f base l

(* stringOfList: ('a -> string) -> 'a list -> string
 *
 * This function takes in a function that will convert each
 * the result of the function upon the elements into string.
 *)

let stringOfList f l = "[" ^ (sepConcat "; " (List.map f l)) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a list
 *
 * This is a curried function that takes in an input x
 * and returns a list filled with n elements with x as the element.
 *
 *)

let rec clone x n = 
   if (n <= 0) then []        (* Check for 0 or negative n *)
   else
      [x] @ (clone x (n-1))   (* Append target to list *)


(* padZero : int list -> int list -> int list * int list
 *
 * This function takes in two lists and adds zeros to make
 * the lists equal.
 *)

let rec padZero l1 l2 = 
   let size1 = List.length l1 in	(* Get l1 Length *)
   let size2 = List.length l2 in        (* Get l2 Length *)
   if size1 < size2 then 		(* Compare Sizes *)
      let diff1 = size2 - size1 in	
      ((clone 0 diff1) @ l1, l2)	(* Return lists with difference *)
   else if size2 < size1 then
      let diff2 = size1 - size2 in
      (l1, (clone 0 diff2) @ l2)
   else
      (l1,l2)


(* removeZero : int list -> int list
 *
 * Takes in an int list and removes all 
 * prefix zeros.
 *)

let rec removeZero l = 
   match l with
      | [] -> []
      | h::t -> if h = 0 then
                   removeZero t
                else
                   l
(* bigAdd : int list -> int list -> int list
 *
 * It takes two int lists and performs addition per index
 * and returns the sum in list form.
 *
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
                let (c,d) = x in         (* Obtain the tuple *)
                let (y,z) = a in         (* Get Accumulator *)
                let num = c + d + y in   (* Add the numbers *)
                if num >= 10 then        (* Check the sum of the two numbers *)
                   (1, (num-10)::z)
                else
                   (0, num::z)
                in
    let base = (0,[]) in                (* Base Case Starts with 0 and empty list *)
    let args =                          (* Reverse both lists and then combine them *)
               let l1' = List.rev (0::l1) in (* So args returns a list of int tuples *)
               let l2' = List.rev (0::l2) in
               List.combine l1' l2' in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))


(* multByDigit : int -> int list -> int list
 * 
 * This function basically multiplies a digit
 * with a big Integer and returns the product of 
 * that big Integer with a list of int.
 *)

let rec mulByDigit i l = 
   match i with 
      | 0 -> []
      | 1 -> l
      | _ -> bigAdd (mulByDigit (i-1) l) l   (* Add l i times *)



(* bigMul : int list -> int list -> int list
 *
 * This function multiplies two big Integers
 * and returns its product in Big Integer Form
 *)


let bigMul l1 l2 = 
  let f a x = 
              let (numList,mulDigit) = x in
              let (place, soFar) = a in
              let product1 = mulByDigit mulDigit numList in
              let product2 = mulByDigit place product1 in 
              let newSoFar = bigAdd soFar product2 in
              (place*10, newSoFar) in
  let base = (1,[]) in
  let args =                                  (* Clone the multiplicant by the total number of multipliers *)
             let l1' = clone l1 (List.length l2) in
             let l2' = List.rev l2 in
             List.combine l1' l2' in		      (* Make tuples of multiplicant by multiplier *)
  let (_, res) = List.fold_left f base args in
    res
