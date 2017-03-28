(* 
 *expr.ml 
 *cse130
 *based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

(* Datatype expr
 * 
 * This is a data type to draw interesting images.
 *)
type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  (* Additional operators for build2*)
  | InversePos of expr   (* 1/(x+2)*)
  | InverseNeg of expr    (* 1/x-2) *)
  | ProductRootPos of expr * expr * expr (* sqrt(|a * b * c|) *)
  | ProductRootNeg of expr * expr * expr (* -sqrt (|a * b * c|) *)

(* exprToString : expr -> string 
 *
 * This function takes in a expr data type and returns it in string form.
 *)
let rec exprToString e =
   match e with
      | VarX -> "x"
      | VarY -> "y"
      | Sine x -> "sin(pi*" ^ exprToString x ^ ")"
      | Cosine y -> "cos(pi*" ^ exprToString y ^ ")"
      | Average (a,b) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ "))/2"
      | Times (a,b) -> exprToString a ^ "*" ^ exprToString b
      | Thresh (a,b,c,d) -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ exprToString d ^ ")"
      (* Addional operators for build2*)
      | InversePos a -> "1 / (" ^ exprToString a ^ " + 2)"
      | InverseNeg a -> "1 / (" ^ exprToString a ^ " - 2)"
      | ProductRootPos (a,b,c) -> "sqrt(" ^ exprToString a ^ "*" ^ exprToString b ^ "*" ^ exprToString c ^ ")"
      | ProductRootNeg (a,b,c) -> "-sqrt(" ^ exprToString a ^ "*" ^ exprToString b ^ "*" ^ exprToString c ^ ")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
(* Build Functions for the additional operators *)
let buildInversePos(e)             = InversePos(e)
let buildInverseNeg(e)             = InverseNeg(e)
let buildProductRootPos(e1,e2,e3)  = ProductRootPos(e1,e2,e3)
let buildProductRootNeg (e1,e2,e3) = ProductRootNeg(e1,e2,e3)


let pi = 4.0 *. atan 1.0

(* eval : expr * float * float -> float
 * 
 * This function takes in (e,x,y) and it evaluates expression e
 * at (x,y);
 *)
let rec eval (e,x,y) = 
   match e with
      | VarX -> x
      | VarY -> y
      | Sine a -> sin (pi *. eval (a,x,y))
      | Cosine a -> cos (pi *. eval (a,x,y))
      | Average (a,b) -> (eval (a,x,y) +. eval (b,x,y)) /. 2.0
      | Times (a,b) -> (eval (a,x,y) *. eval (b,x,y))
      | Thresh(a,b,c,d) -> if eval (a,x,y) < eval (b,x,y) then eval (c,x,y)
                           else eval (d,x,y)
      (* Additional operators for build2 *)
      | InversePos(a) -> (1.0 /. (eval (a,x,y) +. 2.0))
      | InverseNeg(a) -> (1.0 /. (eval (a,x,y) -. 2.0))
      | ProductRootPos(a,b,c) -> sqrt (abs_float (eval (a,x,y) *. eval(b,x,y) *. eval(c,x,y)))
      | ProductRootNeg(a,b,c) -> -1.0 *. sqrt (abs_float (eval (a,x,y) *. eval(b,x,y) *. eval(c,x,y)))

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
