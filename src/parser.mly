%{
open Printf
open Lexing

let parse_error s = print_endline s
%}

%token SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE EQ AEQ NEQ
%token COMMA
%token WHILE IF THEN ELSE
%token <float> NUM
%token <string> ID
%token PLUS MINUS MULTIPLY DIVIDE LT GT CARET

%left COMMA
%left EQ NEQ LT GT
%left AEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation */
%right CARET
%left IFX
%left ELSE
%left PRECPAREN

%start input
%type <unit> input
%type <string> line exp

%%

input:
/* empty */ { }
| input line { print_endline $2 }

lines:
  line { $1 }
| lines line { $1 ^ " " ^ $2 }

blockornot:
  line { "*{" ^ $1 ^ "}" }
| LBRACE lines RBRACE { "{" ^ $2 ^ "}" }
;

line:
  SEMICOLON { ";" }
| WHILE LPAREN exp RPAREN blockornot { "while(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot %prec IFX { "if(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot ELSE blockornot { "if(" ^ $3 ^ ") " ^ $5 ^ " else " ^ $7 }
| exp SEMICOLON { $1 ^ ";" }

exp:
  NUM                               { string_of_float $1 }
| ID                                { $1 }
| exp PLUS exp                      { $1 ^ " + " ^ $3 }
| exp MINUS exp                     { $1 ^ " - " ^ $3 }
| exp MULTIPLY exp                  { $1 ^ " * " ^ $3 }
| exp DIVIDE exp                    { $1 ^ " / " ^ $3 }
| MINUS exp %prec NEG               { "-" ^ $2 }
| exp CARET exp                     { $1 ^ " ^ " ^ $3 }
| exp LT exp                        { $1 ^ " < " ^ $3 }
| exp GT exp                        { $1 ^ " > " ^ $3 }
| exp EQ exp                        { $1 ^ " == " ^ $3 }
| exp NEQ exp                       { $1 ^ " != " ^ $3 }
| IF exp THEN exp ELSE exp          { "if " ^ $2 ^ " then " ^ $4 ^ " else " ^ $6 }
| LPAREN exp RPAREN %prec PRECPAREN { "(" ^ $2 ^ ")" }
| exp COMMA exp                     { $1 ^ ", " ^ $3 }

%%
