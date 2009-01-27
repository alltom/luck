
open Ast

exception Compile_error

type typ =
  ArrayType of typ
| RefType of typ
| IntType | FloatType | BoolType | StringType

type data =
  ArrayData of data array ref
| RefData of data ref
| IntData of int ref
| BoolData of bool ref
| FloatData of float ref
| StringData of string ref

type instruction =
  PrintInstr of data list

module Context = Map.Make(String)

exception Compiler_error of string (* something went wrong internally *)
exception Not_implemented of string
exception Redeclaration
exception Undeclared_variable of string (* name of undeclared variable *)

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | BoolType -> "bool"
  | FloatType -> "float"
  | StringType -> "string"

(* returns the converted data type, an initialized data storage location,
   and the instructions to execute to finish initialization (for non-primitives
   and arrays). ALL arrays (even those defined like "int a[10]") have size 0.
   before the initialization statements are executed. *)
let rec instantiate_type asttype =
  match asttype with
    Type("int", false, _, []) -> (IntType, IntData(ref 0), [])
  | Type("float", false, _, []) -> (FloatType, FloatData(ref 0.0), [])
  | Type("string", false, _, []) -> (StringType, StringData(ref ""), [])
  | Type(t, true, s, a) ->
      let (t', d, s) = instantiate_type (Type(t, false, s, a)) in
      (RefType t', RefData(ref d), s)
  | Type(t, false, s, h::r) ->
      let (t', d, s) = instantiate_type (Type(t, false, s, r)) in
      (ArrayType t', ArrayData (ref (Array.make 0 d)), s)
  | _ -> raise (Not_implemented "cannot instantiate this type")

(* returns a context, and its initialization code *)
let rec build_context cntxt decls =
  let rec loop cntxt instrs decls =
    match decls with
      (name, t) :: rest ->
        let (t', d, i) = instantiate_type t in
        loop (Context.add name (t', d) cntxt) i rest
    | [] -> (cntxt, instrs)
  in loop cntxt [] decls

(* if overwrite is true, variables in c2 overwrite those in c1.
   otherwise, an exception is thrown if the same variable is
   defined in both contexts *)
let combine_cntxts overwrite c1 c2 =
  let add name entry c' =
    if not overwrite && (Context.mem name c') then
      raise Redeclaration
    else
      Context.add name entry c'
  in
  Context.fold add c2 c1

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
    Declaration decls ->
      let (c, i) = build_context Context.empty decls in
      (c, Comma(List.map (fun (name, _) -> Var name) decls), i)
  | Array exps -> list_helper Context.empty [] exps (fun exps' -> Array exps')
  | UnaryExpr (op, e1) -> unary_helper e1 (fun e1' -> UnaryExpr(op, e1'))
  | BinaryExpr (op, e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> BinaryExpr(op, e1', e2'))
  | Member (e1, m) -> unary_helper e1 (fun e1' -> Member(e1', m))
  | FunCall (e1, args) ->
      let (c, e1', i) = extract_expr_cntxt e1 in
      list_helper c i args (fun args' -> FunCall(e1', args'))
  | Cast (e1, t) -> unary_helper e1 (fun e1' -> Cast (e1', t))
  | Spork e1 -> unary_helper e1 (fun e1' -> Spork e1')
  | Trinary (e1, e2, e3) -> trinary_helper e1 e2 e3 (fun e1' e2' e3' -> Trinary(e1', e2', e3'))
  | Comma exps -> list_helper Context.empty [] exps (fun exps' -> Comma exps')
  | _ -> (Context.empty, expr, [])

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);". A copy of the statement
   with used declarations removed is also returned. For example: "<<< 4 => int a >>>"
   becomes "<<< 4 => a >>>". *)
let rec extract_stmt_cntxt local_cntxt stmt =
  match stmt with
    ExprStatement e -> let (c, e', i) = extract_expr_cntxt e in (c, ExprStatement e', i)
  | Print args ->
      let (c, e, i) = extract_expr_cntxt (Comma args) in
      (match e with
         Comma args' -> (c, Print args', i)
       | _ -> raise (Compiler_error "extract_expr_cntxt returned wrong expr"))
  | _ -> (Context.empty, stmt, [])

let compile_expr cntxt expr =
  match expr with
    Declaration decls -> raise (Compiler_error "declaration wasn't extracted earlier")
  | NullExpression -> ([], BoolData(ref true)) (* used only in for() w/blank exprs *)
  | Int i -> ([], IntData(ref i))
  | Float f -> ([], FloatData(ref f))
  | Bool b -> ([], BoolData(ref b))
  | String s -> ([], StringData(ref s))
  | Var name ->
      (try let (_, d) = Context.find name cntxt in ([], d)
       with Not_found -> raise (Undeclared_variable name))
  | _ -> raise (Not_implemented ("cannot compile this expression: " ^ (string_of_expr expr)))

let compile_stmt parent_cntxt local_cntxt stmt =
  let (subcntxt, stmt', init_instrs) = extract_stmt_cntxt local_cntxt stmt in
  let cntxt = combine_cntxts true parent_cntxt subcntxt in
  let instrs =
    match stmt' with
      NullStatement -> []
    | Print args ->
        let compiled_exprs = List.map (fun e -> compile_expr cntxt e) args in
        let instrs = List.fold_left (fun i (i', _) -> i @ i') [] compiled_exprs in
        let args' = List.map (fun (_, d) -> d) compiled_exprs in
        instrs @ [PrintInstr args']
    | _ -> []
  in
  (subcntxt, init_instrs @ instrs)

let compile (AST(fns, classes, stmts)) =
  let parent_cntxt = Context.empty in (* TODO: should be context containing fns and classes *)
  let local_cntxt = ref Context.empty in
  let compile_stmt instrs stmt =
    let (cntxt', instrs') = compile_stmt parent_cntxt !local_cntxt stmt in
    local_cntxt := combine_cntxts false !local_cntxt cntxt';
    instrs @ instrs'
  in
  List.fold_left compile_stmt [] stmts
