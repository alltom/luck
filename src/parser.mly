%{ type expr = Int of int | Add of expr * expr | Mul of expr * expr %}

%token <int> INT
%token PLUS MINUS TIMES OPEN CLOSE EOF

%start expr1
%type <expr> expr1

%left PLUS MINUS
%left TIMES DIVIDE

%%

expr:
| INT             { Int $1 }
| OPEN expr CLOSE { $2 }
| expr PLUS expr  { Add($1, $3) }
| expr MINUS expr { Add($1, Mul(Int(-1), $3)) }
| expr TIMES expr { Mul($1, $3) }
;

expr1:
| expr EOF { $1 }
;
