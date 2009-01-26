
exception Compile_error

type typ =
  ArrayType of typ
| RefType of typ
| IntType | FloatType | StringType

module Context = Map.Make(String)

exception Type_declaration

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | FloatType -> "float"
  | StringType -> "string"

let rec convert_type asttype =
  match asttype with
    Ast.Type("int", false, _, []) -> IntType
  | Ast.Type("float", false, _, []) -> FloatType
  | Ast.Type("string", false, _, []) -> StringType
  | Ast.Type(t, true, s, a) -> RefType(convert_type (Ast.Type(t, false, s, a)))
  | Ast.Type(t, false, s, h::r) -> ArrayType(convert_type (Ast.Type(t, false, s, r)))
  | _ -> raise Type_declaration

let rec build_context cntxt decls =
  match decls with
    (name, t) :: rest ->
      build_context (Context.add name (convert_type t) cntxt) rest
    | [] -> cntxt

(* if overwrite is true, variables in c2 overwrite those in c2.
   otherwise, an exception is thrown if the same variable is
   defined in both contexts *)
let combine_cntxts overwrite c1 c2 =
  c2

let rec get_expr_cntxt expr =
  match expr with
    Ast.Declaration decls -> build_context Context.empty decls
  | Ast.Chuck (e1, e2) -> combine_cntxts false (get_expr_cntxt e1) (get_expr_cntxt e2)
  | _ -> Context.empty

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);" *)
let rec get_stmt_cntxt stmt =
  match stmt with
    Ast.ExprStatement e -> get_expr_cntxt e
  | Ast.Print e -> get_expr_cntxt e
  | _ -> Context.empty

let compile cntxt stmt =
  let subcntxt = get_stmt_cntxt stmt in
  (subcntxt, [])
