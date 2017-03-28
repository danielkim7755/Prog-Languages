{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse 
  | [' ' '\t' '\r' '\n']	{ token lexbuf }

  | "("				{ LPAREN }
  | ")"				{ RPAREN }

  | "+"				{ PLUS }
  | "-"				{ MINUS }
  | "*"				{ MUL }
  | "/"				{ DIV }
  | "<"				{ LT }
  | "<="			{ LE }
  | "!="			{ NE }
  | "&&"			{ AND }
  | "||" 			{ OR }

  | "let"			{ LET }
  | "rec"			{ REC }
  | "=" 			{ EQ }
  | "in"			{ IN }
  | "fun"			{ FUN }
  | "->"			{ ARROW }
  | "if"			{ IF }
  | "then"			{ THEN }
  | "else"			{ ELSE }

  | "true" 			{ TRUE }
  | "false" 			{ FALSE }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
				{ Id(str) }
  | ['0'-'9']+ as num		{ Num(int_of_string num) }

  
  | eof       			{ EOF }
  | _          		 	{ raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
 
