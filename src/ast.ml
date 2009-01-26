
type
  array_size = Dynamic | Fixed of expr and

  (* type: base type * reference type? * static? * array depth *)
  typ = Type of string * bool * bool * array_size list and

  (* declaration of one variable *)
  vardecl = string * typ and

  (* declaration of many variables *)
  decl = vardecl list and

  expr =
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
  and

  stmt =
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
  and

  func = Function of typ * string * decl * stmt list and

  (* public? * name * extended classes * functions * body statements *)
  clas = Class of bool * string * string list * func list * stmt list and

  ast = AST of func list * clas list * stmt list

let string_of_type (Type(name, reference, static, arrdep)) =
  (if static then "static " else "")
    ^ name
    ^ (if reference then " @ " else "")
    ^ (String.concat "" (List.map (function Dynamic -> "[]" | Fixed n -> "[x]") arrdep))

let ast_summary (AST(fns, classes, stmts)) =
  let ip pref str = print_endline (pref ^ str) in
  let function_summary pref (Function(typ, name, decl, stmts)) =
    let p = ip pref in
    p ("function: " ^ name);
    p ("  type: " ^ (string_of_type typ))
  in
  let class_summary pref (Class(public, name, parents, fns, stmts)) =
    let p = ip pref in
    p ((if public then "public " else "") ^ "class: " ^ name);
    p ("  parents: " ^ (String.concat ", " parents));
    p "  methods:";
    List.iter (function_summary (pref ^ "    ")) fns
  in
  List.iter (class_summary "") classes;
  print_endline "functions:";
  List.iter (function_summary "  ") fns
