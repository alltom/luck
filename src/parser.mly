%{
open Printf
open Lexing
open Ast

let parse_error s =
  printf "syntax error at line %d, character %d: %s\n"
    (symbol_start_pos ()).pos_lnum
    (symbol_start_pos ()).pos_cnum
    s

(* converts an expression with commas at top level to a list of the
   sub-expressions *)
let rec commas_to_list e =
  match e with
    Comma (e1, e2) -> (commas_to_list e1) @ (commas_to_list e2)
  | _ -> [e]
%}

%token EOF
%token SEMICOLON PERIOD
%token CHUCK UNCHUCK UPCHUCK ATCHUCK MINUSCHUCK PLUSCHUCK
%token DOLLAR CCOLON SPORK BANG QUESTION COLON
%token LARROWS RARROWS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK EQ NEQ
%token AMPAMP PIPEPIPE
%token COMMA AT
%token WHILE UNTIL DO IF ELSE FOR FUN RETURN PUBLIC STATIC CLASS EXTENDS
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
%type <Ast.ast> input

%%

input:
  units EOF { $1 }
;

units:
/* empty */ { AST([], [], []) }
| units statement { match $1 with AST(f, c, s) -> AST(f, c, s @ [$2]) }
| units func { match $1 with AST(f, c, s) -> AST(f @ [$2], c, s) }
| units clas { match $1 with AST(f, c, s) -> AST(f, c @ [$2], s) }
;

statements:
  statement { [$1] }
| statements statement { $1 @ [$2] }
;

blockornot:
  statement { [$1] }
| block { $1 }
;

block:
  LBRACE statements RBRACE { $2 }
| LBRACE RBRACE { [] }
;

statement:
  SEMICOLON { NullStatement }
| exp SEMICOLON { ExprStatement($1) }
| RETURN SEMICOLON { Return }
| RETURN exp SEMICOLON { ValuedReturn($2) }
| LARROWS exp RARROWS SEMICOLON { Print($2) }
| WHILE LPAREN exp RPAREN blockornot { While($3, $5) }
| DO block WHILE LPAREN exp RPAREN SEMICOLON { Do($2, $5) }
| UNTIL LPAREN exp RPAREN blockornot { Until($3, $5) }
| IF LPAREN exp RPAREN blockornot %prec IFX { If($3, $5, []) }
| IF LPAREN exp RPAREN blockornot ELSE blockornot { If($3, $5, $7) }
| FOR LPAREN optional_exp SEMICOLON optional_exp SEMICOLON optional_exp RPAREN blockornot { For($3, $5, $7, $9) }
;

typ:
  ID { Type($1, false, false, 0) }
| ID AT { Type($1, true, false, 0) }
| STATIC ID { Type($2, false, true, 0) }
| STATIC ID AT { Type($2, true, true, 0) }
;

declarator:
  ID { ($1, 0) }
| declarator LBRACK RBRACK { match $1 with (n, c) -> (n, c+1) }
| declarator LBRACK exp RBRACK { match $1 with (n, c) -> (n, c+1) }
;

declarator_list:
  declarator { [$1] }
| declarator_list COMMA declarator { $1 @ [$3] }
;

optional_exp:
  { NullExpression }
| exp { $1 }
;

exp:
  contained_exp   { $1 }
| uncontained_exp { $1 }
;

contained_exp:
  INT                               { Int($1) }
| FLOAT                             { Float($1) }
| BOOL                              { Bool($1) }
| ID                                { Var($1) }
| STRING                            { String($1) }
| LPAREN exp RPAREN                 { $2 }
| LBRACK exp RBRACK                 { Array(commas_to_list $2) }
| contained_exp PERIOD ID           { Member($1, $3) }
| MINUS contained_exp %prec NEG     { ArithNegation($2) }
| BANG contained_exp %prec NEG      { Negation($2) }
| contained_exp PLUSPLUS            { PostInc($1) }
| PLUSPLUS contained_exp            { PreInc($2) }
| contained_exp MINUSMINUS          { PostDec($1) }
| MINUSMINUS contained_exp          { PreDec($2) }
| contained_exp LPAREN exp RPAREN   { FunCall($1, (commas_to_list $3)) }
| contained_exp LPAREN RPAREN       { FunCall($1, []) }
| contained_exp LBRACK exp RBRACK   { Subscript($1, $3) }
;

uncontained_exp:
  exp CHUCK exp                  { Chuck($1, $3) }
| exp UNCHUCK exp                { Unchuck($1, $3) }
| exp UPCHUCK exp                { Upchuck($1, $3) }
| exp ATCHUCK exp                { Atchuck($1, $3) }
| exp MINUSCHUCK exp             { Minuschuck($1, $3) }
| exp PLUSCHUCK exp              { Pluschuck($1, $3) }
| exp DOLLAR typ                 { Cast($1, $3) }
| exp CCOLON exp                 { Time($1, $3) }
| SPORK exp                      { Spork($2) }
| exp PLUS exp                   { Plus($1, $3) }
| exp MINUS exp                  { Minus($1, $3) }
| exp MULTIPLY exp               { Multiply($1, $3) }
| exp DIVIDE exp                 { Divide($1, $3) }
| exp PERCENT exp                { Modulo($1, $3) }
| exp CARET exp                  { Exponentiate($1, $3) }
| exp LT exp                     { LessThan($1, $3) }
| exp LEQ exp                    { LessThanOrEqualTo($1, $3) }
| exp GT exp                     { GreaterThan($1, $3) }
| exp GEQ exp                    { GreaterThanOrEqualTo($1, $3) }
| exp EQ exp                     { Equals($1, $3) }
| exp NEQ exp                    { NotEquals($1, $3) }
| exp AMPAMP exp                 { BinaryAnd($1, $3) }
| exp PIPEPIPE exp               { BinaryOr($1, $3) }
| exp QUESTION exp COLON exp     { Trinary($1, $3, $5) }
| typ declarator_list %prec DECL { match $1 with Type(t, r, s, _) -> Declaration(List.map (fun (n, c) -> (n, Type(t, r, s, c))) $2) }
| exp COMMA exp                  { Comma($1, $3) }
;

/* FUNCTIONS */

functyp:
  ID {  Type($1, false, false, 0) }
| ID AT { Type($1, true, false, 0) }
| functyp LBRACK RBRACK { match $1 with Type(n, r, s, c) -> Type(n, r, s, c+1) }
;

paramlist:
  typ declarator { match $1 with Type(t, r, s, _) -> match $2 with (n, c) -> [(n, Type(t, r, s, c))] }
| paramlist COMMA typ declarator { match $3 with Type(t, r, s, _) -> match $4 with (n, c) -> $1 @ [(n, Type(t, r, s, c))] }
;

func:
  FUN functyp ID LPAREN RPAREN block { Function($2, $3, [], $6) }
| FUN functyp ID LPAREN paramlist RPAREN block { Function($2, $3, $5, $7) }
;

/* CLASSES */

id_list:
  ID { [$1] }
| id_list COMMA ID { $3::$1 }

extend:
  EXTENDS id_list { $2 }
;

pub:
  { false }
| PUBLIC { true }
;

clasblock:
  LBRACE clasblocks RBRACE { $2 }
| LBRACE RBRACE { ([], []) }
;

clasblocks:
  statement { ([], [$1]) }
| func { ([$1], []) }
| clasblocks statement { match $1 with (f, s) -> (f, s @ [$2]) }
| clasblocks func { match $1 with (f, s) -> ($2::f, s) }
;

clas:
  pub CLASS ID clasblock { match $4 with (f, s) -> Class($1, $3, [], f, s) }
| pub CLASS ID extend clasblock { match $5 with (f, s) -> Class($1, $3, $4, f, s) }
;

%%
