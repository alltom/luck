%{
open Printf
open Lexing

let var_table = Hashtbl.create 16

let parse_error s = print_endline s
%}

%token NEWLINE
%token LPAREN RPAREN EQ AEQ NEQ
%token COMMA
%token <float> NUM
%token PLUS MINUS MULTIPLY DIVIDE LT GT CARET
%token <string> VAR
%token <float->float> FNCT

%left COMMA
%left EQ NEQ
%nonassoc LT GT
%left AEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation */
%right CARET

%start input
%type <unit> input

%%

input:
             { }
| input line { }

line:
  NEWLINE       { }
| exp NEWLINE   { printf "\t%.10g\n" $1; flush stdout }
| error NEWLINE { }

exp:
  NUM                 { $1 }
| VAR                 { try Hashtbl.find var_table $1 with Not_found -> printf "no such variable '%s'\n" $1; 0.0 }
| VAR AEQ exp         { Hashtbl.replace var_table $1 $3; $3 }
| FNCT LPAREN exp RPAREN { $1 $3 }
| exp PLUS exp        { $1 +. $3 }
| exp MINUS exp       { $1 -. $3 }
| exp MULTIPLY exp    { $1 *. $3 }
| exp DIVIDE exp
	{
		if $3 <> 0.0 then
			$1 /. $3
		else (
			let start_pos = Parsing.rhs_start_pos 3 in
			let end_pos = Parsing.rhs_end_pos 3 in
			printf "%d.%d-%d.%d: division by zero"
			  start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
			  end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);
			1.0
		)
	}
| MINUS exp %prec NEG { -. $2 }
| exp CARET exp       { $1 ** $3 }
| exp LT exp          { if $1 < $3 then 1.0 else 0.0 }
| exp GT exp          { if $1 > $3 then 1.0 else 0.0 }
| exp EQ exp          { if $1 == $3 then 1.0 else 0.0 }
| exp NEQ exp         { if $1 <> $3 then 1.0 else 0.0 }
| LPAREN exp RPAREN   { $2 }
| exp COMMA exp       { $3 }

%%
