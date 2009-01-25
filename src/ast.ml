
(* declaration: base type * (variable name * array depth) list *)
type decl =
  Decl of string * (string * int) list
| ReferenceDecl of string * (string * int) list

type expr =
  Int of int
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
| Cast of expr * expr
| Time of expr * expr
| Spork of expr
| Plus of expr * expr
| Minus of expr * expr
| Multiply of expr * expr
| Divide of expr * expr
| Modulo of expr * expr
| Exponent of expr * expr
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
| Commas of expr * expr

type stmt =
  ValuedReturn of expr
| Return
| Print of expr
| While of expr * expr list
| Do of expr * expr list
| Until of expr * expr list
| If of expr * expr list * expr list
| For of expr * expr * expr * expr list
