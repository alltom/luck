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
| '\n'                     { incr_lineno lexbuf; token lexbuf }
| ';'                      { SEMICOLON }
| "fun"                    { FUN }
| "public"                  { PUBLIC }
| "class"                  { CLASS }
| "extends"                { EXTENDS }
| "while"                  { WHILE }
| "if"                     { IF }
| "then"                   { THEN }
| "else"                   { ELSE }
| digit+
| "." digit+
| digit+ "." digit* as num { NUM (float_of_string num) }
| ident as name            { ID(name) }
| "=>"                     { CHUCK }
| '@'                      { AT }
| '+'                      { PLUS }
| '-'                      { MINUS }
| '*'                      { MULTIPLY }
| '/'                      { DIVIDE }
| '^'                      { CARET }
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
| eof                      { raise End_of_file }
