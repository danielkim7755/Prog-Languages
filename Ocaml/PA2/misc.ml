(*CSE 130: Programming Assignment 2
 * misc.ml
 *)


(* assoc : int * string * (string * int) -> int
 * 
 * The assoc function takes in a triple (a, b, c) where
 * a is a default value, b is a string that is being searched for
 * and c is a list of string * int pairs. Then the function
 * recursively searches each pair of the list until there is a
 * match with b and an element of the string in c.The int of the
 * correlating string is returned.
 *
 *)

let rec assoc (d,k,l) = 
   match l with
      | [] -> d              (* If list is empty return default*)
      | (s,i)::t ->          
         if s = k then i     (* Compare k with the string element of the head of the tuple list*)
         else assoc(d,k,t)


(* removeDuplicates : 'a list -> 'a list
 * 
 * The removeDuplicates function takes in any types of list
 * and returns the a with all the duplicated elements removed.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        (* Chech if head is a part of the seen list 
           If yes, then just seen if not con h to seen
         *)
        let seen' = (if List.mem h seen then seen
                     else h::seen)
        in
        let rest' = t in    (* Pass the rest of the list *)
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)

(* wwhile ('a -> 'a * bool) * 'a -> 'a
 *
 * The wwhile function that takes in a function f that returns
 * a int * bool pair and an 
 * input b, where the function f runs on b and returns a
 * pair of (b', c) where c is a boolean. This will recursively
 * occur until c returns false.
 *)
let rec wwhile (f,b) =
   let (b',c) = f b in  (* Run f on b *)
      match c with      
         | false -> b'  (* Return b' if false *)
         | true -> wwhile (f,b')   (* Recursively run f on b' *)
      


(* fixpoint : ('a -> 'a) * 'a -> 'a
 * 
 * The fixpoint function takes in an input (f,b)
 * where f is a function that 'a -> 'a and b is the input
 * which updates b with f(b) until b = f(b) and then returns
 * b.
 *)

(* checker : int -> int * bool
 * 
 * This function checks if b = f(b) and returns values
 * so that it is compatible to the wwhile function. 
 *
 * Returns (b, false) if b = f(b) else (the new value, true).
 *)
let fixpoint (f,b) = 
wwhile (
(  let checker b' = 
      let x = f b' in          (* Check if b = f(b) *)
         if x = b' then (b', false)
         else (x, true)
         in
         checker)
, b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
