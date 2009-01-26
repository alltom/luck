
exception Compile_error

type typ =
  ArrayType of typ
| RefType of typ
| IntType | FloatType | StringType

type data =
  ArrayData of data array ref
| RefData of data ref
| IntData of int ref
| FloatData of float ref
| StringData of string ref

module Context = Map.Make(String)

exception Type_declaration
exception Variable_initialization
exception Redeclaration

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | FloatType -> "float"
  | StringType -> "string"

(* returns the converted data type, and an initialize data storage location.
   ALL arrays (even those defined like "int a[10]") have size 0. *)
let rec instantiate_type asttype =
  match asttype with
    Ast.Type("int", false, _, []) -> (IntType, IntData(ref 0))
  | Ast.Type("float", false, _, []) -> (FloatType, FloatData(ref 0.0))
  | Ast.Type("string", false, _, []) -> (StringType, StringData(ref ""))
  | Ast.Type(t, true, s, a) ->
      let (t', d) = instantiate_type (Ast.Type(t, false, s, a)) in
      (RefType t', RefData(ref d))
  | Ast.Type(t, false, s, h::r) ->
      let (t', d) = instantiate_type (Ast.Type(t, false, s, r)) in
      (ArrayType t', ArrayData (ref (Array.make 0 d)))
  | _ -> raise Type_declaration

let rec build_context cntxt decls =
  match decls with
    (name, t) :: rest ->
      build_context (Context.add name (instantiate_type t) cntxt) rest
  | [] -> cntxt

(* if overwrite is true, variables in c2 overwrite those in c2.
   otherwise, an exception is thrown if the same variable is
   defined in both contexts *)
let combine_cntxts overwrite c1 c2 =
  let add name entry c' =
    if not overwrite && (Context.mem name c') then
      raise Redeclaration
    else
      Context.add name entry c'
  in
  Context.fold add c1 c2

(* extracts declarations from expr, returning a context with the
  declared variables, and an expr with declarations replaced by the
  variables themselves.
     "int a" becomes "a"
     "int a[], b" becomes "a, b" *)
let rec extract_expr_cntxt expr =
  let unary_helper e1 f =
    let (c, e') = extract_expr_cntxt e1 in
    (c, f e')
  in
  let binary_helper e1 e2 f =
    let (c1, e1') = extract_expr_cntxt e1 in
    let (c2, e2') = extract_expr_cntxt e2 in
    ((combine_cntxts false c1 c2), f e1' e2')
  in
  match expr with
    Ast.Declaration decls -> (build_context Context.empty decls, expr)
  | Ast.Array exps ->
      let cntxt = ref Context.empty in
      let exps' = List.map
        (fun e ->
           let (c', e') = extract_expr_cntxt e in
           cntxt := combine_cntxts false !cntxt c';
           e')
        exps
      in
      (!cntxt, (Ast.Array exps'))
  | Ast.UnaryExpr (op, e1) -> unary_helper e1 (fun e1' -> Ast.UnaryExpr(op, e1'))
  | Ast.BinaryExpr (op, e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Ast.BinaryExpr(op, e1', e2'))
  | _ -> (Context.empty, expr)

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);" *)
let rec extract_stmt_cntxt stmt =
  match stmt with
    Ast.ExprStatement e -> let (c, e') = extract_expr_cntxt e in (c, Ast.ExprStatement e')
  | Ast.Print e -> let (c, e') = extract_expr_cntxt e in (c, Ast.Print e')
  | _ -> (Context.empty, stmt)

(* rough outline:
   - extract variable declarations relevant to outer context
   - replace those declarations with the variables themselves
   - ...
*)
let compile cntxt stmt =
  let (subcntxt, stmt') = extract_stmt_cntxt stmt in
  (subcntxt, [])
