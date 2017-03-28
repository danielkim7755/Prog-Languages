(* CSE 130: Programming Assignment 1
 * misc.ml
 *)


(* sumList : int list -> int 
   Takes in an integer list and returns the sum of the list.
   
   If the input list is empty, it will return 0 or else
   it will remove the head and recursively add it to the head.
*) 

let rec sumList l =
   match l with
      | [] -> 0
      | (h::t) -> (sumList t) + h


(* digitsOfInt : int -> int list 
   Takes an integer and returns an int list of its digits. 
   
   Uses a helper function that takes in an integer and an int list
   which recursively breaks down the integer to 0 while storing each
   digit to the list.

   Digits are broken down by dividing the int by 10.
   While the digits stored are accumulated by the mod operator by 10.

   A 0 will return a list containing a zero
   Negative numbers will return a list containing the input negative number.

*)

let digitsOfInt n = 
   let rec helper n result =
      if (n < 0) then (n::result)
      else if (n = 0) then result
      else helper (n/10) ((n mod 10)::result)
   in
   match n with
      | 0 -> [0]
      | _ -> helper n []

 

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
   Takes in an int and returns an int of its additive persistence.

   It recursively adds the sum of its digits until the number 
   consists of a single digit. 
   The number of times addition was performed will be returned.
   
   e.g. additivePersistence 783 = 7+8+3 = 18 = 1 + 8 = 9 so it should return 2.

 *)
let rec additivePersistence n = 
   if (n < 10) then 0
   else 1 + (additivePersistence (sumList (digits n)))


(* digitalRoot : int -> int
   Takes in an int and returns the digital root.

   It recursively adds the sum of its digits until the sum
   consists of a single digit.

   e.g. digitalRoot 783 = 18 = 9. so it will return 9
 *)

let rec digitalRoot n = 
   if (n < 10) then n
   else (digitalRoot (sumList (digits n)))




(* listReverse : 'a list -> 'a list
   Takes in a list and returns a list with the elements in 
   reverse order.;

   This function uses a helper function that takes in two 
   lists where the first list is the remaining of the original
   list while the second list is the reversed list holder.

   The helper function recursively takes the head off of the
   original list and cons it into the resultHolder thus creating
   a reverse list.
 *)

let listReverse l = 
   let rec revHelper l resultHolder = 
      match l with
         | [] -> resultHolder
         | (h::t) -> revHelper t (h::resultHolder)
      in
      revHelper l []
         
   

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
   This function takes in a string and returns a boolean to see if the 
   input string is a palidrome.
   
   A palindrome is a string that is spelled the same both backwards and forwards.

   This function compares two list: explode <input string> and (listReverse (explode <input string>))
   Returns true if both lists are the same.
   
   e.g palindrome "racecar" -> return true

 *)
let palindrome w = 
   match w with
   | "" -> true
   | _ -> let (x,y) = ((explode w), (listReverse (explode w)))
                    in
                    if x = y then true
                    else false

(************** Add Testing Code Here ***************)

