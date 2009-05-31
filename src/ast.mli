
type
  array_size = Dynamic | Fixed of expr and

  (* type: base type * reference type? * static? * array depth *)
  typ = Type of string * bool * bool * array_size list and

  (* declaration of one variable *)
  vardecl = string * typ and

  (* declaration of many variables *)
  decl = vardecl list and
  
  unary_op =
    ArithNegation (* -x *)
  | Negation (* !x *)
  | PreInc | PostInc (* ++x and x++ *)
  | PreDec | PostDec (* --x and x-- *)
  and
  
  binary_op =
    Chuck | Unchuck | Upchuck | Atchuck | Minuschuck | Pluschuck
  | Plus | Minus | Multiply | Divide | Modulo | Exponentiate
  | LessThan | LessThanOrEqualTo
  | GreaterThan | GreaterThanOrEqualTo
  | Equals | NotEquals
  | BinaryAnd | BinaryOr
  and

  expr =
    Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Var of string
  | Array of expr list
  | Comma of expr list
  | UnaryExpr of unary_op * expr
  | BinaryExpr of binary_op * expr * expr
  | Member of expr * string
  | FunCall of expr * expr list
  | Cast of expr * typ
  | Spork of expr
  | Trinary of expr * expr * expr
  | Subscript of expr * expr
  | Time of expr * expr
  | Declaration of decl
  and

  stmt =
    NullStatement
  | ExprStatement of expr
  | ValuedReturn of expr
  | Return
  | Break
  | Print of expr list
  | While of expr * stmt list
  | Do of stmt list * expr
  | Until of expr * stmt list
  | Repeat of expr * stmt list
  | If of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  and

  func = Function of typ * string * decl * stmt list and

  (* public? * name * extended classes * functions * body statements *)
  clas = Class of bool * string * string list * func list * stmt list and

  ast = func list * clas list * stmt list

val string_of_type : typ -> string

val string_of_expr : expr -> string

val string_of_stmt : stmt -> string

val print_ast_summary : ast -> unit
