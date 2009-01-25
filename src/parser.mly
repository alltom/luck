%{
open Printf
open Lexing

let parse_error s = print_endline s
%}

%token SEMICOLON PERIOD
%token CHUCK UPCHUCK
%token DOLLAR CCOLON SPORK BANG
%token LARROWS RARROWS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK EQ NEQ
%token COMMA AT
%token WHILE IF ELSE FUN PUBLIC CLASS EXTENDS
%token <float> FLOAT
%token <int> INT
%token <string> ID
%token <string> STRING
%token PLUS MINUS MULTIPLY DIVIDE LT GT CARET

%left CHUCK UPCHUCK
%left COMMA
%left EQ NEQ LT GT
%left PLUS MINUS
%left MULTIPLY DIVIDE
%right CARET
%left DOLLAR
%left CCOLON
%left SPORK
%left NEG
%left SUBSC
%left IFX
%left ELSE
%left PRECPAREN
%left LPAREN LBRACK
%left PERIOD

%start input
%type <unit> input

%%

input:
/* empty */ { }
| input line { print_endline $2 } /* code */
| input func { print_endline $2 } /* function definition */
| input clas { print_endline $2 } /* class definition */
;

lines:
  line { $1 }
| lines line { $1 ^ " " ^ $2 }
;

/* one line of code, or a block */
blockornot:
  line { "*{" ^ $1 ^ "}" }
| block { $1 }
;

block:
  LBRACE lines RBRACE { "{" ^ $2 ^ "}" }
| LBRACE RBRACE { "{ }" }
;

line:
  SEMICOLON { ";" }
| exp SEMICOLON { $1 ^ ";" }
| LARROWS exp RARROWS SEMICOLON { "<<< " ^ $2 ^ " >>>;" }
| WHILE LPAREN exp RPAREN blockornot { "while(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot %prec IFX { "if(" ^ $3 ^ ") " ^ $5 }
| IF LPAREN exp RPAREN blockornot ELSE blockornot { "if(" ^ $3 ^ ") " ^ $5 ^ " else " ^ $7 }
;

typ:
  ID { $1 }
| typ AT { $1 ^ "@" }
| typ LBRACK RBRACK { $1 ^ "[]" }
| typ LBRACK exp RBRACK { $1 ^ "[" ^ $3 ^ "]" }
;

exp:
  contained_exp   { $1 }
| uncontained_exp { $1 }

contained_exp:
  INT                               { string_of_int $1 }
| FLOAT                             { string_of_float $1 }
| ID                                { $1 }
| STRING                            { "\"" ^ $1 ^ "\"" }
| LPAREN exp RPAREN %prec PRECPAREN { "(" ^ $2 ^ ")" }
| contained_exp PERIOD ID           { "(" ^ $1 ^ "." ^ $3 ^ ")" }
| MINUS contained_exp %prec NEG     { "-(" ^ $2 ^ ")" }
| BANG contained_exp %prec NEG      { "!(" ^ $2 ^ ")" }
| contained_exp LPAREN exp RPAREN   { $1 ^ "(" ^ $3 ^ ")" }
| contained_exp LPAREN RPAREN       { "(" ^ $1 ^ "())" }
| contained_exp LBRACK exp RBRACK   { $1 ^ "[" ^ $3 ^ "]" }
;

uncontained_exp:
  exp CHUCK exp            { $1 ^ " => " ^ $3 }
| exp UPCHUCK exp          { $1 ^ " =^ " ^ $3 }
| exp DOLLAR typ           { $1 ^ " $ " ^ $3 }
| exp CCOLON typ           { $1 ^ "::" ^ $3 }
| SPORK exp                { "spork ~ " ^ $2 }
| exp PLUS exp             { $1 ^ " + " ^ $3 }
| exp MINUS exp            { $1 ^ " - " ^ $3 }
| exp MULTIPLY exp         { $1 ^ " * " ^ $3 }
| exp DIVIDE exp           { $1 ^ " / " ^ $3 }
| exp CARET exp            { $1 ^ " ^ " ^ $3 }
| exp LT exp               { $1 ^ " < " ^ $3 }
| exp GT exp               { $1 ^ " > " ^ $3 }
| exp EQ exp               { $1 ^ " == " ^ $3 }
| exp NEQ exp              { $1 ^ " != " ^ $3 }
| ID ID                    { $1 ^ " " ^ $2 }
| exp COMMA exp            { $1 ^ ", " ^ $3 }
;

/* FUNCTIONS */

paramlist:
  typ ID { $1 ^ " " ^ $2 }
| paramlist COMMA typ ID { $1 ^ ", " ^ $3 ^ " " ^ $4 }

func:
  FUN typ ID LPAREN RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "() " ^ $6 }
| FUN typ ID LPAREN paramlist RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "(" ^ $5 ^ ") " ^ $7 }
;

/* CLASSES */

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

clasblock:
  LBRACE claslines RBRACE { "{" ^ $2 ^ "}" }
| LBRACE RBRACE { "{ }" }
;

claslines:
  line { $1 }
| func { $1 }
| claslines line { $1 ^ " " ^ $2 }
| claslines func { $2 }
;

clas:
  pub CLASS ID clasblock { $1 ^ "class " ^ $3 ^ " " ^ $4 }
| pub CLASS ID extend clasblock { $1 ^ "class " ^ $3 ^ " " ^ $4 ^ " " ^ $5 }
;

%%
