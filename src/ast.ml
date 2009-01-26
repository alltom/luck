
(* type: base type * reference type? * static? * array depth *)
type typ = Type of string * bool * bool * int

(* declaration of one variable *)
type vardecl = string * typ

(* declaration of many variables *)
type decl = vardecl list

type expr =
  NullExpression
| Int of int
| Float of float
| Bool of bool
| String of string
| Var of string
| Array of expr list
| ArithNegation of expr
| Negation of expr
| PreInc of expr
| PostInc of expr
| PreDec of expr
| PostDec of expr
| Member of expr * string
| FunCall of expr * expr list
| Subscript of expr * expr
| Chuck of expr * expr
| Unchuck of expr * expr
| Upchuck of expr * expr
| Atchuck of expr * expr
| Minuschuck of expr * expr
| Pluschuck of expr * expr
| Cast of expr * typ
| Time of expr * expr
| Spork of expr
| Plus of expr * expr
| Minus of expr * expr
| Multiply of expr * expr
| Divide of expr * expr
| Modulo of expr * expr
| Exponentiate of expr * expr
| LessThan of expr * expr
| LessThanOrEqualTo of expr * expr
| GreaterThan of expr * expr
| GreaterThanOrEqualTo of expr * expr
| Equals of expr * expr
| NotEquals of expr * expr
| BinaryAnd of expr * expr
| BinaryOr of expr * expr
| Trinary of expr * expr * expr
| Declaration of decl
| Comma of expr * expr

type stmt =
  NullStatement
| ExprStatement of expr
| ValuedReturn of expr
| Return
| Print of expr
| While of expr * stmt list
| Do of stmt list * expr
| Until of expr * stmt list
| If of expr * stmt list * stmt list
| For of expr * expr * expr * stmt list

type func =
  Function of typ * string * decl * stmt list

(* public? * name * extended classes * functions * body statements *)
type clas =
  Class of bool * string * string list * func list * stmt list

type ast = AST of func list * clas list * stmt list
