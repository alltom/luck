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
%token PLUS MINUS MULTIPLY DIVIDE LT GT CARET

%left COMMA
%left EQ NEQ LT GT
%left AEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG /* negation */
%right CARET
%left ELSE

%start input
%type <unit> input line exp

%%

input:
/* empty */  { }
| input line { print_endline "^_~" }

lines:
  line  { }
| lines line { }

line:
  SEMICOLON       { }
| WHILE LPAREN exp RPAREN LBRACE lines RBRACE { }
| IF LPAREN exp RPAREN LBRACE lines RBRACE { }
| IF LPAREN exp RPAREN LBRACE lines RBRACE ELSE LBRACE lines RBRACE { }
| exp SEMICOLON   { }

exp:
  NUM                      { }
| exp PLUS exp             { }
| exp MINUS exp            { }
| exp MULTIPLY exp         { }
| exp DIVIDE exp           { }
| MINUS exp %prec NEG      { }
| exp CARET exp            { }
| exp LT exp               { }
| exp GT exp               { }
| exp EQ exp               { }
| exp NEQ exp              { }
| IF exp THEN exp ELSE exp { }
| LPAREN exp RPAREN        { }
| exp COMMA exp            { }

%%
