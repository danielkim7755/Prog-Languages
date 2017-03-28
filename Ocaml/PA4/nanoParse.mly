%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token TRUE FALSE
%token <string> Id

%token LET REC
%token EQ IN
%token FUN ARROW
%token IF THEN ELSE

%token PLUS MINUS MUL DIV
%token LT LE NE
%token AND OR

%token LPAREN RPAREN

%nonassoc LET FUN IF
%left OR
%left AND
%left EQ NE LT LE
%left PLUS MINUS
%left MUL DIV
%left APP

%start exp 
%type <Nano.expr> exp

%%

exp: 
  | LET Id EQ exp IN exp	{ Let($2,$4,$6) }
  | LET REC Id EQ exp IN exp	{ Letrec($3,$5,$7) }
  | FUN Id ARROW exp		{ Fun($2,$4) }
  | IF exp THEN exp ELSE exp	{ If($2,$4,$6) }
  | expO			{ $1 }

expO:
  | expO OR expO		{ Bin($1,Or,$3) }
  | expA			{ $1 }

expA:
  | expA AND expC		{ Bin($1,And,$3) }
  | expC			{ $1 }

expC:
  | expC EQ expPM		{ Bin($1,Eq,$3) }
  | expC NE expPM		{ Bin($1,Ne,$3) }
  | expC LT expPM		{ Bin($1,Lt,$3) }
  | expC LE expPM		{ Bin($1,Le,$3) }
  | expPM			{ $1 }

expPM:
  | expPM PLUS expMD		{ Bin($1,Plus,$3) }
  | expPM MINUS expMD		{ Bin($1,Minus,$3) }
  | expMD			{ $1 }

expMD:
  | expMD MUL expAPP		{ Bin($1,Mul,$3) }
  | expMD DIV expAPP		{ Bin($1,Div,$3) }
  | expAPP			{ $1 }
 
expAPP:
  | expAPP atom			{ App($1,$2) }
  | atom			{ $1 }

atom:
  | LPAREN exp RPAREN		{ $2 }
  | Num				{ Const($1) }
  | Id				{ Var($1) }
  | TRUE			{ True }
  | FALSE			{ False }


 
