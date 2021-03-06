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
   sub-expressions ("a, b, c" becomes [a, b, c]) *)
let rec commas_to_list e =
  match e with
    Comma exps -> exps
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
%token WHILE UNTIL REPEAT DO IF ELSE FOR FUN RETURN BREAK PUBLIC STATIC CLASS EXTENDS
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
/* empty */ { ([], [], []) }
| units statement { match $1 with (f, c, s) -> (f, c, s @ [$2]) }
| units func { match $1 with (f, c, s) -> (f @ [$2], c, s) }
| units clas { match $1 with (f, c, s) -> (f, c @ [$2], s) }
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
| BREAK SEMICOLON { Break }
| LARROWS exp RARROWS SEMICOLON { Print(commas_to_list $2) }
| WHILE LPAREN exp RPAREN blockornot { While($3, $5) }
| DO block WHILE LPAREN exp RPAREN SEMICOLON { Do($2, $5) }
| UNTIL LPAREN exp RPAREN blockornot { Until($3, $5) }
| REPEAT LPAREN exp RPAREN blockornot { Repeat($3, $5) }
| IF LPAREN exp RPAREN blockornot %prec IFX { If($3, $5, []) }
| IF LPAREN exp RPAREN blockornot ELSE blockornot { If($3, $5, $7) }
| FOR LPAREN optional_exp SEMICOLON optional_exp SEMICOLON optional_exp RPAREN blockornot { For($3, $5, $7, $9) }
;

typ:
  ID { Type($1, false, false, []) }
| ID AT { Type($1, true, false, []) }
| STATIC ID { Type($2, false, true, []) }
| STATIC ID AT { Type($2, true, true, []) }
;

declarator:
  ID { ($1, []) }
| declarator LBRACK RBRACK { match $1 with (n, c) -> (n, c @ [Dynamic]) }
| declarator LBRACK exp RBRACK { match $1 with (n, c) -> (n, c @ [Fixed $3]) }
;

declarator_list:
  declarator { [$1] }
| declarator_list COMMA declarator { $1 @ [$3] }
;

optional_exp:
  { Bool(true) } /* handy for empty expressions in for loops */
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
| MINUS contained_exp %prec NEG     { UnaryExpr(ArithNegation, $2) }
| BANG contained_exp %prec NEG      { UnaryExpr(Negation, $2) }
| contained_exp PLUSPLUS            { UnaryExpr(PostInc, $1) }
| PLUSPLUS contained_exp            { UnaryExpr(PreInc, $2) }
| contained_exp MINUSMINUS          { UnaryExpr(PostDec, $1) }
| MINUSMINUS contained_exp          { UnaryExpr(PreDec, $2) }
| contained_exp LPAREN exp RPAREN   { FunCall($1, (commas_to_list $3)) }
| contained_exp LPAREN RPAREN       { FunCall($1, []) }
| contained_exp LBRACK exp RBRACK   { Subscript($1, $3) }
;

uncontained_exp:
  exp CHUCK exp                  { BinaryExpr(Chuck, $1, $3) }
| exp UNCHUCK exp                { BinaryExpr(Unchuck, $1, $3) }
| exp UPCHUCK exp                { BinaryExpr(Upchuck, $1, $3) }
| exp ATCHUCK exp                { BinaryExpr(Atchuck, $1, $3) }
| exp MINUSCHUCK exp             { BinaryExpr(Minuschuck, $1, $3) }
| exp PLUSCHUCK exp              { BinaryExpr(Pluschuck, $1, $3) }
| exp DOLLAR typ                 { Cast($1, $3) }
| exp CCOLON exp                 { Time($1, $3) }
| SPORK exp                      { Spork($2) }
| exp PLUS exp                   { BinaryExpr(Plus, $1, $3) }
| exp MINUS exp                  { BinaryExpr(Minus, $1, $3) }
| exp MULTIPLY exp               { BinaryExpr(Multiply, $1, $3) }
| exp DIVIDE exp                 { BinaryExpr(Divide, $1, $3) }
| exp PERCENT exp                { BinaryExpr(Modulo, $1, $3) }
| exp CARET exp                  { BinaryExpr(Exponentiate, $1, $3) }
| exp LT exp                     { BinaryExpr(LessThan, $1, $3) }
| exp LEQ exp                    { BinaryExpr(LessThanOrEqualTo, $1, $3) }
| exp GT exp                     { BinaryExpr(GreaterThan, $1, $3) }
| exp GEQ exp                    { BinaryExpr(GreaterThanOrEqualTo, $1, $3) }
| exp EQ exp                     { BinaryExpr(Equals, $1, $3) }
| exp NEQ exp                    { BinaryExpr(NotEquals, $1, $3) }
| exp AMPAMP exp                 { BinaryExpr(BinaryAnd, $1, $3) }
| exp PIPEPIPE exp               { BinaryExpr(BinaryOr, $1, $3) }
| exp QUESTION exp COLON exp     { Trinary($1, $3, $5) }
| typ declarator_list %prec DECL { match $1 with Type(t, r, s, _) -> Declaration(List.map (fun (n, c) -> (n, Type(t, r, s, c))) $2) }
| exp COMMA exp                  { Comma((commas_to_list $1) @ (commas_to_list $3)) }
;

/* FUNCTIONS */

functyp:
  ID { Type($1, false, false, []) }
| STATIC ID { Type($2, false, true, []) }
| ID AT { Type($1, true, false, []) }
| STATIC ID AT { Type($2, true, true, []) }
| functyp LBRACK RBRACK { match $1 with Type(n, r, s, c) -> Type(n, r, s, c @ [Dynamic]) }
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
| id_list COMMA ID { $1 @ [$3] }

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
| clasblocks func { match $1 with (f, s) -> (f @ [$2], s) }
;

clas:
  pub CLASS ID clasblock { match $4 with (f, s) -> Class($1, $3, [], f, s) }
| pub CLASS ID extend clasblock { match $5 with (f, s) -> Class($1, $3, $4, f, s) }
;

%%
