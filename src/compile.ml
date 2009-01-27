
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

exception Compiler_error (* something went wrong internally *)
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

(* returns the converted data type, an initialized data storage location,
   and the instructions to execute to finish initialization (for non-primitives
   and arrays). ALL arrays (even those defined like "int a[10]") have size 0.
   before the initialization statements are executed. *)
let rec instantiate_type asttype =
  match asttype with
    Ast.Type("int", false, _, []) -> (IntType, IntData(ref 0), [])
  | Ast.Type("float", false, _, []) -> (FloatType, FloatData(ref 0.0), [])
  | Ast.Type("string", false, _, []) -> (StringType, StringData(ref ""), [])
  | Ast.Type(t, true, s, a) ->
      let (t', d, s) = instantiate_type (Ast.Type(t, false, s, a)) in
      (RefType t', RefData(ref d), s)
  | Ast.Type(t, false, s, h::r) ->
      let (t', d, s) = instantiate_type (Ast.Type(t, false, s, r)) in
      (ArrayType t', ArrayData (ref (Array.make 0 d)), s)
  | _ -> raise Type_declaration

(* returns a context, and its initialization code *)
let rec build_context cntxt decls =
  let rec loop cntxt instrs decls =
    match decls with
      (name, t) :: rest ->
        let (t', d, i) = instantiate_type t in
        loop (Context.add name (t', d) cntxt) i rest
    | [] -> (cntxt, instrs)
  in loop cntxt [] decls

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
  declared variables, an expr with declarations replaced by the
  variables themselves, and the code to initialize them.
     "int a" becomes "a"
     "int a[], b" becomes "a, b" *)
let rec extract_expr_cntxt expr =
  let unary_helper e1 f =
    let (c, e', i) = extract_expr_cntxt e1 in
    (c, f e', i)
  in
  let binary_helper e1 e2 f =
    let (c1, e1', i1) = extract_expr_cntxt e1 in
    let (c2, e2', i2) = extract_expr_cntxt e2 in
    ((combine_cntxts false c1 c2), f e1' e2', i1 @ i2)
  in
  let trinary_helper e1 e2 e3 f =
    let (c1, e1', i1) = extract_expr_cntxt e1 in
    let (c2, e2', i2) = extract_expr_cntxt e2 in
    let (c3, e3', i3) = extract_expr_cntxt e3 in
    ((combine_cntxts false c1 (combine_cntxts false c2 c3)), f e1' e2' e3', i1 @ i2 @ i3)
  in
  let list_helper cntxt instrs lst f =
    let cntxt' = ref cntxt in
    let lst' = List.map
      (fun e ->
         let (c', e', i) = extract_expr_cntxt e in
         cntxt' := combine_cntxts false !cntxt' c';
         e')
      lst
    in
    (!cntxt', f lst', instrs @ [])
  in
  match expr with
    Ast.Declaration decls ->
      let (c, i) = build_context Context.empty decls in
      (c, Ast.Comma(List.map (fun (name, _) -> Ast.Var name) decls), i)
  | Ast.Array exps -> list_helper Context.empty [] exps (fun exps' -> Ast.Array exps')
  | Ast.UnaryExpr (op, e1) -> unary_helper e1 (fun e1' -> Ast.UnaryExpr(op, e1'))
  | Ast.BinaryExpr (op, e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Ast.BinaryExpr(op, e1', e2'))
  | Ast.Member (e1, m) -> unary_helper e1 (fun e1' -> Ast.Member(e1', m))
  | Ast.FunCall (e1, args) ->
      let (c, e1', i) = extract_expr_cntxt e1 in
      list_helper c i args (fun args' -> Ast.FunCall(e1', args'))
  | Ast.Cast (e1, t) -> unary_helper e1 (fun e1' -> Ast.Cast (e1', t))
  | Ast.Spork e1 -> unary_helper e1 (fun e1' -> Ast.Spork e1')
  | Ast.Trinary (e1, e2, e3) -> trinary_helper e1 e2 e3 (fun e1' e2' e3' -> Ast.Trinary(e1', e2', e3'))
  | _ -> (Context.empty, expr, [])

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);" *)
let rec extract_stmt_cntxt stmt =
  match stmt with
    Ast.ExprStatement e -> let (c, e', i) = extract_expr_cntxt e in (c, Ast.ExprStatement e', i)
  | Ast.Print args ->
      let (c, e, i) = extract_expr_cntxt (Ast.Comma args) in
      (match e with
         Ast.Comma args' -> (c, Ast.Print args', i)
       | _ -> raise Compiler_error)
  | _ -> (Context.empty, stmt, [])

(* rough outline:
   - extract variable declarations relevant to outer context
   - replace those declarations with the variables themselves
   - ...
*)
let compile cntxt stmt =
  let (subcntxt, stmt', instrs) = extract_stmt_cntxt stmt in
  (subcntxt, instrs)
