{
	open Parser
	open Lexing

	let incr_lineno lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- { pos with
			pos_lnum = pos.pos_lnum + 1;
			pos_bol = pos.pos_cnum
		}
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
rule token = parse
  [' ' '\t']               { token lexbuf }
| '\n' '\r'? | '\r' '\n'?  { incr_lineno lexbuf; token lexbuf }
| "//" [^'\n' '\r']*       { token lexbuf }
| "/*" ('*' [^'/'] | [^'*'])* "*/"      { token lexbuf }
| '"' (([^ '"' '\\']| '\\'_)* as s) '"' { STRING (s) }
| "true"                   { BOOL(true) }
| "false"                  { BOOL(false) }
| ';'                      { SEMICOLON }
| "return"                 { RETURN }
| "fun"                    { FUN }
| "public"                 { PUBLIC }
| "class"                  { CLASS }
| "extends"                { EXTENDS }
| "while"                  { WHILE }
| "if"                     { IF }
| "else"                   { ELSE }
| "for"                    { FOR }
| "." digit+
| digit+ "." digit* as num { FLOAT (float_of_string num) }
| "0x" digit+ as num       { INT (int_of_string num) }
| digit+ as num            { INT (int_of_string num) }
| ident as name            { ID(name) }
| "-=>"                    { MINUSCHUCK }
| "+=>"                    { PLUSCHUCK }
| "=^"                     { UPCHUCK }
| "@=>"                    { ATCHUCK }
| "=>"                     { CHUCK }
| "=<"                     { UNCHUCK }
| '@'                      { AT }
| '$'                      { DOLLAR }
| "::"                     { CCOLON }
| "spork ~"                { SPORK }
| "++"                     { PLUSPLUS }
| '+'                      { PLUS }
| "--"                     { MINUSMINUS }
| '-'                      { MINUS }
| '*'                      { MULTIPLY }
| '/'                      { DIVIDE }
| '^'                      { CARET }
| '%'                      { PERCENT }
| "<<<"                    { LARROWS }
| ">>>"                    { RARROWS }
| "<="                     { LEQ }
| '<'                      { LT }
| ">="                     { GEQ }
| '>'                      { GT }
| "=="                     { EQ }
| "!="                     { NEQ }
| "&&"                     { AMPAMP }
| "||"                     { PIPEPIPE }
| '('                      { LPAREN }
| ')'                      { RPAREN }
| '{'                      { LBRACE }
| '}'                      { RBRACE }
| '['                      { LBRACK }
| ']'                      { RBRACK }
| ','                      { COMMA }
| '.'                      { PERIOD }
| '!'                      { BANG }
| '?'                      { QUESTION }
| ':'                      { COLON }
| eof                      { raise End_of_file }
