%{
open Printf
open Lexing

let parse_error s = print_endline s
%}

%token SEMICOLON PERIOD
%token CHUCK UNCHUCK UPCHUCK ATCHUCK MINUSCHUCK PLUSCHUCK
%token DOLLAR CCOLON SPORK BANG QUESTION COLON
%token LARROWS RARROWS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK EQ NEQ
%token AMPAMP PIPEPIPE
%token COMMA AT
%token WHILE IF ELSE FOR FUN PUBLIC CLASS EXTENDS
%token <float> FLOAT
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token <string> STRING
%token PLUS MINUS MULTIPLY DIVIDE PERCENT LT GT LEQ GEQ CARET
%token PLUSPLUS MINUSMINUS

%left CHUCK UNCHUCK UPCHUCK ATCHUCK MINUSCHUCK PLUSCHUCK
%left DECL /* int a, b, c */
%left COMMA
%left QUESTION COLON
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left PERCENT
%right CARET
%left DOLLAR
%left CCOLON
%left SPORK
%left AMPAMP PIPEPIPE
%left NEG
%nonassoc PLUSPLUS MINUSMINUS
%left SUBSC
%left IFX
%left ELSE
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
| FOR LPAREN optional_exp SEMICOLON optional_exp SEMICOLON optional_exp RPAREN blockornot { "for(" ^ $3 ^ "; " ^ $5 ^ "; " ^ $7 ^ ") " ^ $9 }
;

typ:
  ID { $1 }
| ID AT { $1 ^ " @" }
;

declarator:
  ID { $1 }
| declarator LBRACK RBRACK { $1 ^ "[]" }
| declarator LBRACK exp RBRACK { $1 ^ "[" ^ $3 ^ "]" }
;

declarator_list:
  declarator { $1 }
| declarator_list COMMA declarator { $1 ^ ", " ^ $3 }
;

optional_exp:
  { "" }
| exp { $1 }
;

exp:
  contained_exp   { $1 }
| uncontained_exp { $1 }
;

contained_exp:
  INT                               { string_of_int $1 }
| FLOAT                             { string_of_float $1 }
| BOOL                              { string_of_bool $1 }
| ID                                { $1 }
| STRING                            { "\"" ^ $1 ^ "\"" }
| LPAREN exp RPAREN                 { "(" ^ $2 ^ ")" }
| LBRACK exp RBRACK                 { "[" ^ $2 ^ "]" }
| contained_exp PERIOD ID           { "(" ^ $1 ^ "." ^ $3 ^ ")" }
| MINUS contained_exp %prec NEG     { "-(" ^ $2 ^ ")" }
| BANG contained_exp %prec NEG      { "!(" ^ $2 ^ ")" }
| contained_exp PLUSPLUS            { "(" ^ $1 ^ ")++" }
| PLUSPLUS contained_exp            { "++(" ^ $2 ^ ")" }
| contained_exp MINUSMINUS          { "(" ^ $1 ^ ")--" }
| MINUSMINUS contained_exp          { "--(" ^ $2 ^ ")" }
| contained_exp LPAREN exp RPAREN   { $1 ^ "(" ^ $3 ^ ")" }
| contained_exp LPAREN RPAREN       { "(" ^ $1 ^ "())" }
| contained_exp LBRACK exp RBRACK   { $1 ^ "[" ^ $3 ^ "]" }
;

uncontained_exp:
  exp CHUCK exp            { $1 ^ " => " ^ $3 }
| exp UNCHUCK exp          { $1 ^ " =< " ^ $3 }
| exp UPCHUCK exp          { $1 ^ " =^ " ^ $3 }
| exp ATCHUCK exp          { $1 ^ " @=> " ^ $3 }
| exp MINUSCHUCK exp       { $1 ^ " -=> " ^ $3 }
| exp PLUSCHUCK exp        { $1 ^ " +=> " ^ $3 }
| exp DOLLAR typ           { $1 ^ " $ " ^ $3 }
| exp CCOLON typ           { $1 ^ "::" ^ $3 }
| SPORK exp                { "spork ~ " ^ $2 }
| exp PLUS exp             { $1 ^ " + " ^ $3 }
| exp MINUS exp            { $1 ^ " - " ^ $3 }
| exp MULTIPLY exp         { $1 ^ " * " ^ $3 }
| exp DIVIDE exp           { $1 ^ " / " ^ $3 }
| exp PERCENT exp          { $1 ^ " % " ^ $3 }
| exp CARET exp            { $1 ^ " ^ " ^ $3 }
| exp LT exp               { $1 ^ " < " ^ $3 }
| exp LEQ exp              { $1 ^ " <= " ^ $3 }
| exp GT exp               { $1 ^ " > " ^ $3 }
| exp GEQ exp              { $1 ^ " >= " ^ $3 }
| exp EQ exp               { $1 ^ " == " ^ $3 }
| exp NEQ exp              { $1 ^ " != " ^ $3 }
| exp AMPAMP exp           { $1 ^ " && " ^ $3 }
| exp PIPEPIPE exp         { $1 ^ " || " ^ $3 }
| exp QUESTION exp COLON exp { "(" ^ $1 ^ ") ? (" ^ $3 ^ ") : (" ^ $5 ^ ")" }
| typ declarator_list %prec DECL { "(" ^ $1 ^ " " ^ $2 ^ ")" }
| exp COMMA exp            { $1 ^ ", " ^ $3 }
;

/* FUNCTIONS */

functyp:
  ID { $1 }
| ID AT { $1 ^ "@" }
| typ LBRACK RBRACK { $1 ^ "[]" }
;

paramlist:
  typ declarator { $1 ^ " " ^ $2 }
| paramlist COMMA typ declarator { $1 ^ ", " ^ $3 ^ " " ^ $4 }
;

func:
  FUN functyp ID LPAREN RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "() " ^ $6 }
| FUN functyp ID LPAREN paramlist RPAREN block { "fun " ^ $2 ^ " " ^ $3 ^ "(" ^ $5 ^ ") " ^ $7 }
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
