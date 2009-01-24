{
	open Parser
	open Lexing

	let incr_lineno lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- { pos with
			pos_lnum = pos.pos_lnum + 1;
			pos_bol = pos.pos_cnum
		}

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

	let fun_table = create_hashtable 16 [
		("sin", sin);
		("cos", cos);
		("tan", tan);
		("asin", asin);
		("acos", acos);
		("atan", atan);
		("log", log);
		("exp", exp);
		("sqrt", sqrt);
	]
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
  [' ' '\t']               { token lexbuf }
| '\n'                     { incr_lineno lexbuf; token lexbuf }
| ';'                      { SEMICOLON }
| digit+
| "." digit+
| digit+ "." digit* as num { NUM (float_of_string num) }
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
| '='                      { AEQ }
| ','                      { COMMA }
| "if"                     { IF }
| "then"                     { THEN }
| "else"                   { ELSE }
| ident ident_num* as word { try let f = Hashtbl.find fun_table word in FNCT f with Not_found -> VAR word }
| _                        { token lexbuf }
| eof                      { raise End_of_file }
