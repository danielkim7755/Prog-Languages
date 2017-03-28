exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | NilExpr ->
        "Null"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup : string * env -> value
 *
 * Checks for the most recent binding of the input variable from
 * the environment list and returns its value.
 *)

let lookup (x,evn) = 
   let res = listAssoc (x,evn)
   in
   match res with
      | Some a -> a
      | _ -> raise (MLFailure ("Variable not bound: " ^ x))

(* eval : env * expr -> value
 *
 * This function takes in a pair of the environment and an expression
 * in order to evaluate an ML-nano expression and return the value.
 *)

let rec eval (evn,e) = 
   match e with
      | Const a -> Int a 
      | True -> Bool true
      | False -> Bool false
      | Var a -> lookup (a,evn)
      | Bin (x,y,z) ->
        (
         let a = eval (evn,x) in
         let b = eval (evn,z) in
         match y with				(* Perform Operations *)
            | Plus ->  (match (a,b) with
                         | (Int a',Int b') -> Int (a' + b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Minus -> (match (a,b) with
                         | (Int a', Int b') -> Int (a' - b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Mul ->   (match (a,b) with
                         | (Int a', Int b') -> Int (a' * b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Div ->   (match (a,b) with
                         | (Int a', Int b') -> Int (a' / b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Eq ->    (match (a,b) with
                         | (Int a', Int b') -> Bool (a' = b')
                         | (Bool a', Bool b') -> Bool (a' = b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Ne ->    (match (a,b) with
                         | (Int a', Int b') -> Bool (a' != b')
                         | (Bool a', Bool b') -> Bool (a' != b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Lt ->    (match (a,b) with
                         | (Int a', Int b') -> Bool (a' < b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Le ->    (match (a,b) with
                         | (Int a', Int b') -> Bool (a' <= b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | And ->   (match (a,b) with
                         | (Bool a', Bool b') -> Bool (a' && b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | Or ->    (match (a,b) with
                         | (Bool a', Bool b') -> Bool (a' || b')
                         | _ -> raise (MLFailure ("Error: Wrong type.")))
            | _ -> raise (MLFailure ("Operator does not exist."))
        )
      | If (x,y,z) -> (
           let x' = eval (evn,x) in
           match x' with
             | Bool a -> if a = true then eval (evn, y)
                         else eval (evn, z)
             | _ -> raise (MLFailure ("Error: Evaluation is not a Bool")))
      | Let (x,y,z) -> (                              	(* let x = y in z *)
           let x' = eval (evn,y) in
           let evn' = (x,x')::evn in
           eval (evn',z))
      | Letrec (x,y,z) -> (
           let x' = eval (evn,y) in
           let fin_evn = 
              match x' with
                 | Closure (evn',None,var,bod) -> (x,Closure (evn',Some x,var,bod))::evn
                 | _ -> (x,x')::evn
           in
           eval (fin_evn,z))
      | App (e1,e2) -> (
           let arg = eval (evn,e2) in
           let Closure (evn,name,var,bod) = eval (evn,e1) in
           let fin_evn =
              match name with
                 | Some a -> (a,Closure (evn,name,var,bod))::(var,arg)::evn
                 | None -> (var,arg)::evn
           in
           eval (fin_evn,bod)
        )
      | Fun (x,e) -> Closure (evn,None,x,e)
      | _ -> raise (MLFailure ("Error: Not Implemented"))
                    


(**********************     Testing Code  ******************************)
