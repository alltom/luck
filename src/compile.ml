
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

type binary_op =
  ChuckOp | UnchuckOp | UpchuckOp | AtchuckOp | MinuschuckOp | PluschuckOp
| PlusOp | MinusOp | MultiplyOp | DivideOp | ModuloOp | ExponentiateOp
| LessThanOp | LessThanOrEqualToOp
| GreaterThanOp | GreaterThanOrEqualToOp
| EqualsOp | NotEqualsOp
| BinaryAndOp | BinaryOrOp

let binop_of_astbinop op =
  match op with
    Chuck -> ChuckOp | Unchuck -> UnchuckOp
  | Upchuck -> UpchuckOp | Atchuck -> AtchuckOp
  | Minuschuck -> MinuschuckOp | Pluschuck -> PluschuckOp
  | Plus -> PlusOp | Minus -> MinusOp | Multiply -> MultiplyOp | Divide -> DivideOp
  | Modulo -> ModuloOp | Exponentiate -> ExponentiateOp
  | LessThan -> LessThanOp | LessThanOrEqualTo -> LessThanOrEqualToOp
  | GreaterThan -> GreaterThanOp | GreaterThanOrEqualTo -> GreaterThanOrEqualToOp
  | Equals -> EqualsOp | NotEquals -> NotEqualsOp
  | BinaryAnd -> BinaryAndOp | BinaryOr -> BinaryOrOp

type instruction =
  PrintInstr of data list
| IntBinaryOpInstr of binary_op * int ref * int ref * data ref
| FloatBinaryOpInstr of binary_op * float ref * float ref * data ref
| BoolBinaryOpInstr of binary_op * bool ref * bool ref * data ref
| StringBinaryOpInstr of binary_op * string ref * string ref * data ref

module Context = Map.Make(String)

exception Compiler_error of string (* something went wrong internally *)
exception Not_implemented of string
exception Redeclaration
exception Undeclared_variable of string (* name of undeclared variable *)
exception Type_mismatch of string

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | BoolType -> "bool"
  | FloatType -> "float"
  | StringType -> "string"

let strings_of_cntxt cntxt =
  Context.fold (fun name (t, d) lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

let rec string_of_data d =
  match d with
    ArrayData elems -> "array"
  | RefData d' -> "ref " ^ (string_of_data !d')
  | IntData i -> "int " ^ (string_of_int !i)
  | BoolData b -> "bool " ^ (string_of_bool !b)
  | FloatData f -> "float " ^ (string_of_float !f)
  | StringData s -> "string \"" ^ !s ^ "\""

let string_of_binary_op op =
  match op with
    ChuckOp -> "=>"
  | UnchuckOp -> "=<"
  | UpchuckOp -> "=^"
  | AtchuckOp -> "@=>"
  | MinuschuckOp -> "-=>"
  | PluschuckOp -> "+=>"
  | PlusOp -> "+" | MinusOp -> "-" | MultiplyOp -> "*" | DivideOp -> "/"
  | ModuloOp -> "%"
  | ExponentiateOp -> "^"
  | LessThanOp -> "<" | LessThanOrEqualToOp -> "<="
  | GreaterThanOp -> ">" | GreaterThanOrEqualToOp -> ">="
  | EqualsOp -> "==" | NotEqualsOp -> "!="
  | BinaryAndOp -> "&&" | BinaryOrOp -> "||"

let string_of_instr i =
  match i with
    PrintInstr datas -> "print(" ^ (String.concat ", " (List.map string_of_data datas)) ^ ")"
  | IntBinaryOpInstr (op, l, r, o) -> "intop(" ^ (string_of_int !l) ^ " " ^ (string_of_binary_op op) ^ " " ^ (string_of_int !r) ^ ")"
  | FloatBinaryOpInstr (op, l, r, o) -> "floatop(" ^ (string_of_float !l) ^ " " ^ (string_of_binary_op op) ^ " " ^ (string_of_float !r) ^ ")"
  | BoolBinaryOpInstr (op, l, r, o) -> "boolop(" ^ (string_of_bool !l) ^ " " ^ (string_of_binary_op op) ^ " " ^ (string_of_bool !r) ^ ")"
  | StringBinaryOpInstr (op, l, r, o) -> "stringop(\"" ^ !l ^ "\" " ^ (string_of_binary_op op) ^ " \"" ^ !r ^ "\")"

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
      (ArrayType t', ArrayData (ref (Array.make 0 d)), s) (* TODO: instructions for intitializing properly *)
  | _ -> raise (Not_implemented "cannot instantiate this type")

(* returns a context, and its initialization code *)
let rec build_context decls =
  let rec loop cntxt instrs decls =
    match decls with
      (name, t) :: rest ->
        let (t', d, i) = instantiate_type t in
        loop (Context.add name (t', d) cntxt) i rest
    | [] -> (cntxt, instrs)
  in loop Context.empty [] decls

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

(* given a list of, say exprs, extracts context from them all using
   extractf, returning a tuple of the context, the array of extracted
   expressions, and the initialization instructions *)
let extract_list_cntxt lst extractf =
  let cntxt = ref Context.empty in
  let exprs = ref [] in
  let instrs = ref [] in
  List.iter
    (fun e ->
       let (c', e', i) = extractf e in
       cntxt := combine_cntxts false !cntxt c';
       exprs := !exprs @ [e'];
       instrs := !instrs @ i)
    lst;
  (!cntxt, !exprs, !instrs)

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
  match expr with
    Array exps -> let (c, exps', i) = extract_list_cntxt exps extract_expr_cntxt in (c, Array exps', i)
  | Comma exps -> let (c, exps', i) = extract_list_cntxt exps extract_expr_cntxt in (c, Comma exps', i)
  | UnaryExpr (op, e1) -> unary_helper e1 (fun e1' -> UnaryExpr(op, e1'))
  | BinaryExpr (op, e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> BinaryExpr(op, e1', e2'))
  | Member (e1, m) -> unary_helper e1 (fun e1' -> Member(e1', m))
  | FunCall (e1, args) ->
      let (c1, e1', i1) = extract_expr_cntxt e1 in
      let (c2, args', i2) = extract_list_cntxt args extract_expr_cntxt in
      (combine_cntxts false c1 c2, FunCall(e1', args'), i1 @ i2)
  | Cast (e1, t) -> unary_helper e1 (fun e1' -> Cast (e1', t))
  | Spork e1 -> unary_helper e1 (fun e1' -> Spork e1')
  | Trinary (e1, e2, e3) -> trinary_helper e1 e2 e3 (fun e1' e2' e3' -> Trinary(e1', e2', e3'))
  | Subscript (e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Subscript(e1', e2'))
  | Time (e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Time(e1', e2'))
  | Declaration decls ->
      let (c, i) = build_context decls in
      (match decls with
         (name, _) :: [] -> (c, Var name, i)
       | _ -> (c, Array(List.map (fun (name, _) -> Var name) decls), i))
  | NullExpression | Int _ | Float _ | Bool _ | String _ | Var _ -> (Context.empty, expr, [])

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);". A copy of the statement
   with used declarations removed is also returned. For example: "<<< 4 => int a >>>"
   becomes "<<< 4 => a >>>". *)
let rec extract_stmt_cntxt local_cntxt stmt =
  match stmt with
    ExprStatement e -> let (c, e', i) = extract_expr_cntxt e in (c, ExprStatement e', i)
  | Print args ->
      let (c, args', i) = extract_list_cntxt args extract_expr_cntxt in
      (c, Print args', i)
  | _ -> (Context.empty, stmt, [])

(* returns the type which best covers t1 and t2 *)
let rec promote_type t1 t2 =
  match (t1, t2) with
    (ArrayType t1', ArrayType t2') -> promote_type t1' t2'
  | (RefType t1', RefType t2') -> promote_type t1' t2'
  | (IntType, IntType) -> IntType
  | (FloatType, FloatType) -> FloatType
  | (BoolType, BoolType) -> BoolType
  | (StringType, StringType) -> StringType
  | _ -> raise (Type_mismatch ("incompatible types " ^ (string_of_type t1) ^ " and " ^ (string_of_type t2)))

let get_type cntxt e =
  IntType (* TODO *)

let make_int d =
  match d with
    IntData i -> ([], i)
  | _ -> ([], ref 0) (* TODO *)

let make_float d =
  match d with
    FloatData f -> ([], f)
  | _ -> ([], ref 0.0) (* TODO *)

let make_bool d =
  match d with
    BoolData b -> ([], b)
  | _ -> ([], ref false) (* TODO *)

let make_string d =
  match d with
    StringData s -> ([], s)
  | _ -> ([], ref "") (* TODO *)

let rec compile_expr cntxt expr =
  match expr with
    NullExpression -> ([], BoolData(ref true)) (* used only in for() w/blank exprs *)
  | Int i -> ([], IntData(ref i))
  | Float f -> ([], FloatData(ref f))
  | Bool b -> ([], BoolData(ref b))
  | String s -> ([], StringData(ref s))
  | Var name ->
      (try let (_, d) = Context.find name cntxt in ([], d)
       with Not_found -> raise (Undeclared_variable name))
  | Array exprs ->
      let pairs = List.map (fun e -> compile_expr cntxt e) exprs in
      let instrs = List.fold_left (fun i (i', _) -> i @ i') [] pairs in
      let arr = Array.of_list (List.map (fun (_, d) -> d) pairs) in
      (instrs, ArrayData(ref arr))
  | Comma exprs ->
      List.fold_left
        (fun (i, d) e -> let (i', d) = compile_expr cntxt e in (i @ i', d))
        ([], IntData(ref 0)) (* the data on the right here is a placeholder, overwritten by fun above *)
        exprs
  | UnaryExpr (op, e1) -> raise (Not_implemented "cannot compile unary expressions")
  | BinaryExpr (op, e1, e2) ->
      let (i1, d1) = compile_expr cntxt e1 in
      let (i2, d2) = compile_expr cntxt e2 in
      let helper out coercefn instrfn =
        let (i1', d1') = coercefn d1 in
        let (i2', d2') = coercefn d2 in
        (i1 @ i2 @ i1' @ i2' @ [instrfn (binop_of_astbinop op) d1' d2' out], out)
      in
      (match promote_type (get_type cntxt e1) (get_type cntxt e2) with
         ArrayType elems -> raise (Not_implemented "cannot compile binary expression with arrays")
       | RefType d -> raise (Not_implemented "cannot compile binary expression with reference types")
       | IntType -> helper (IntData(ref 0)) make_int (fun op' d1' d2' out -> IntBinaryOpInstr(op', d1', d2', ref out))
       | FloatType -> helper (FloatData(ref 0.0)) make_float (fun op' d1' d2' out -> FloatBinaryOpInstr(op', d1', d2', ref out))
       | BoolType -> helper (BoolData(ref false)) make_bool (fun op' d1' d2' out -> BoolBinaryOpInstr(op', d1', d2', ref out))
       | StringType -> helper (StringData(ref "")) make_string (fun op' d1' d2' out -> StringBinaryOpInstr(op', d1', d2', ref out))
       )
  | Member (e1, mem) -> raise (Not_implemented "cannot compile member expressions")
  | FunCall (e1, args) -> raise (Not_implemented "cannot compile function calls")
  | Cast (e1, t) -> raise (Not_implemented "cannot compile casts")
  | Spork e1 -> raise (Not_implemented "cannot compile sporks")
  | Trinary (e1, e2, e3) -> raise (Not_implemented "cannot compile trinary conditionals")
  | Subscript (e1, e2) -> raise (Not_implemented "cannot compile subscription")
  | Time (e1, e2) -> raise (Not_implemented "cannot compile time expressions")
  | Declaration decls -> raise (Compiler_error "declaration wasn't extracted earlier")

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
