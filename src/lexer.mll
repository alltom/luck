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
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
rule token = parse
  [' ' '\t']               { token lexbuf }
| '\n' '\r'? | '\r' '\n'?  { incr_lineno lexbuf; token lexbuf }
| "//" [^'\n' '\r']*       { token lexbuf }
| '"' (([^ '"' '\\']| '\\'_)* as s) '"' { STRING (s) }
| ';'                      { SEMICOLON }
| "fun"                    { FUN }
| "public"                 { PUBLIC }
| "class"                  { CLASS }
| "extends"                { EXTENDS }
| "while"                  { WHILE }
| "if"                     { IF }
| "else"                   { ELSE }
| "." digit+
| digit+ "." digit* as num { FLOAT (float_of_string num) }
| digit+ as num            { INT (int_of_string num) }
| ident as name            { ID(name) }
| "=>"                     { CHUCK }
| '@'                      { AT }
| '$'                      { DOLLAR }
| "::"                     { CCOLON }
| "spork ~"                { SPORK }
| '+'                      { PLUS }
| '-'                      { MINUS }
| '*'                      { MULTIPLY }
| '/'                      { DIVIDE }
| '^'                      { CARET }
| "<<<"                    { LARROWS }
| ">>>"                    { RARROWS }
| '<'                      { LT }
| '>'                      { GT }
| "=="                     { EQ }
| "!="                     { NEQ }
| '('                      { LPAREN }
| ')'                      { RPAREN }
| '{'                      { LBRACE }
| '}'                      { RBRACE }
| '['                      { LBRACK }
| ']'                      { RBRACK }
| ','                      { COMMA }
| '.'                      { PERIOD }
| '!'                      { BANG }
| eof                      { raise End_of_file }
