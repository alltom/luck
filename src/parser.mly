%{
open Printf
open Lexing

let parse_error s = print_endline s
%}

%token SEMICOLON
%token CHUCK
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK EQ NEQ
%token COMMA AT
%token WHILE IF THEN ELSE FUN PUBLIC CLASS EXTENDS
%token <float> NUM
%token <string> ID
%token PLUS MINUS MULTIPLY DIVIDE LT GT CARET

%left CHUCK
%left COMMA
%left EQ NEQ LT GT
%left AEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation */
%left SUBSC
%right CARET
%left IFX
%left ELSE
%left PRECPAREN

%start input
%type <unit> input

%%

input:
/* empty */ { }
| input line { print_endline $2 }
| input func { print_endline $2 }
| input clas { print_endline $2 }
;

lines:
  line { $1 }
| lines line { $1 ^ " " ^ $2 }
;

claslines:
  line { $1 }
| func { $1 }
| claslines line { $1 ^ " " ^ $2 }
| claslines func { $2 }
;

blockornot:
  line { "*{" ^ $1 ^ "}" }
| block { $1 }
;

block:
  LBRACE lines RBRACE { "{" ^ $2 ^ "}" }
| LBRACE RBRACE { "{ }" }
;

clasblock:
  LBRACE claslines RBRACE { "{" ^ $2 ^ "}" }
| LBRACE RBRACE { "{ }" }
;

line:
  SEMICOLON { ";" }
| WHILE LPAREN exp RPAREN blockornot { "while(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot %prec IFX { "if(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot ELSE blockornot { "if(" ^ $3 ^ ") " ^ $5 ^ " else " ^ $7 }
| exp SEMICOLON { $1 ^ ";" }
;

typ:
  ID { $1 }
| typ AT { $1 ^ "@" }
| typ LBRACK RBRACK { $1 ^ "[]" }
| typ LBRACK exp RBRACK { $1 ^ "[" ^ $3 ^ "]" }
;

paramlist:
  typ ID { $1 ^ " " ^ $2 }
| paramlist COMMA typ ID { $1 ^ ", " ^ $3 ^ " " ^ $4 }

func:
  FUN typ ID LPAREN RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "() " ^ $6 }
| FUN typ ID LPAREN paramlist RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "(" ^ $5 ^ ") " ^ $7 }
;

id_list:
  ID { $1 }
| id_list COMMA ID { $1 ^ ", " ^ $3 }

extend:
  EXTENDS id_list { "extends " ^ $2 }
;

pub:
  { "" }
| PUBLIC { "public " }
;

clas:
  pub CLASS ID clasblock { $1 ^ "class " ^ $3 ^ " " ^ $4 }
| pub CLASS ID extend clasblock { $1 ^ "class " ^ $3 ^ " " ^ $4 ^ " " ^ $5 }
;

exp:
  NUM                               { string_of_float $1 }
| ID                                { $1 }
| exp CHUCK exp                     { $1 ^ " => " ^ $3 }
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
| ID ID                             { $1 ^ " " ^ $2 }
| ID LPAREN exp RPAREN              { $1 ^ "(" ^ $3 ^ ")" }
| IF exp THEN exp ELSE exp          { "if " ^ $2 ^ " then " ^ $4 ^ " else " ^ $6 }
| LPAREN exp RPAREN %prec PRECPAREN { "(" ^ $2 ^ ")" }
| ID LBRACK exp RBRACK              { $1 ^ "[" ^ $3 ^ "]" }
| LPAREN exp RPAREN LBRACK exp RBRACK { "(" ^ $2 ^ ")[" ^ $5 ^ "]" }
| exp COMMA exp                     { $1 ^ ", " ^ $3 }
;

%%
