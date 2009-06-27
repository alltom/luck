
open Ast
open Interpreter
open Vm

exception Compile_error of string (* something is wrong with the input *)
exception Compiler_error of string (* something went wrong internally *)
exception Not_implemented of string

let undeclared var = raise (Compile_error ("use of undeclared variable " ^ var))

let builtin_context =
  List.fold_left
    (fun cntxt (name, typ) -> Context.add name typ cntxt)
    Context.empty
    [("now", TimeType); ("samp", DurType); ("second", DurType)]
let builtin_type name = Context.find name builtin_context
let is_builtin name = Context.mem name builtin_context

let rec typ_of_asttype asttype =
  match asttype with
    Type("int", false, _, []) -> IntType
  | Type("float", false, _, []) -> FloatType
  | Type("string", false, _, []) -> StringType
  | Type("dur", false, _, []) -> DurType
  | _ -> raise (Not_implemented "cannot yet convert this data type")

(* raises an compile exception if the given variable name is reserved (ex: now, second, ...) *)
let raise_if_reserved name =
  if is_builtin name then
    raise (Compile_error ("redeclaration of reserved variable " ^ name))
  else
    name

(* returns a context from a list of declarations *)
let rec build_context decls =
  let rec loop cntxt decls =
    match decls with
      (name, t) :: rest -> loop (Context.add (raise_if_reserved name) (typ_of_asttype t) cntxt) rest
    | [] -> cntxt
  in loop Context.empty decls

(* if overwrite is true, variables in c2 overwrite those in c1.
   otherwise, an exception is thrown if the same variable is
   defined in both contexts *)
let combine_cntxts overwrite c1 c2 =
  let add name entry c' =
    if not overwrite && (Context.mem name c') then
      raise (Compile_error ("redeclaration of " ^ name))
    else
      Context.add name entry c'
  in
  Context.fold add c2 c1

let combine_cntxt_list overwrite lst =
  List.fold_left
    (fun c c' -> combine_cntxts overwrite c c')
    Context.empty
    lst

(* given a list of, say exprs, extracts context from them all using
   extractf, returning a tuple of the context and the array of extracted
   expressions *)
let extract_list_cntxt lst extractf =
  let cntxt = ref Context.empty in
  let exprs = ref [] in
  List.iter
    (fun e ->
       let (c', e') = extractf e in
       cntxt := combine_cntxts false !cntxt c';
       exprs := !exprs @ [e'])
    lst;
  (!cntxt, !exprs)

(* extracts declarations from expr, returning a context with the
  declared variables and an expr with declarations replaced by the
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
  let trinary_helper e1 e2 e3 f =
    let (c1, e1') = extract_expr_cntxt e1 in
    let (c2, e2') = extract_expr_cntxt e2 in
    let (c3, e3') = extract_expr_cntxt e3 in
    ((combine_cntxts false c1 (combine_cntxts false c2 c3)), f e1' e2' e3')
  in
  match expr with
    Array exps -> let (c, exps') = extract_list_cntxt exps extract_expr_cntxt in (c, Array exps')
  | Comma exps -> let (c, exps') = extract_list_cntxt exps extract_expr_cntxt in (c, Comma exps')
  | UnaryExpr (op, e1) -> unary_helper e1 (fun e1' -> UnaryExpr(op, e1'))
  | BinaryExpr (op, e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> BinaryExpr(op, e1', e2'))
  | Member (e1, m) -> unary_helper e1 (fun e1' -> Member(e1', m))
  | FunCall (e1, args) ->
      let (c1, e1') = extract_expr_cntxt e1 in
      let (c2, args') = extract_list_cntxt args extract_expr_cntxt in
      (combine_cntxts false c1 c2, FunCall(e1', args'))
  | Cast (e1, t) -> unary_helper e1 (fun e1' -> Cast (e1', t))
  | Spork e1 -> unary_helper e1 (fun e1' -> Spork e1')
  | Trinary (e1, e2, e3) -> trinary_helper e1 e2 e3 (fun e1' e2' e3' -> Trinary(e1', e2', e3'))
  | Subscript (e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Subscript(e1', e2'))
  | Time (e1, e2) -> binary_helper e1 e2 (fun e1' e2' -> Time(e1', e2'))
  | Declaration decls ->
      let c = build_context decls in
      (match decls with
         (name, _) :: [] -> (c, Var name)
       | _ -> (c, Array(List.map (fun (name, _) -> Var name) decls)))
  | Int _ | Float _ | Bool _ | String _ | Var _ -> (Context.empty, expr)

(* extract declarations from sub-expressions which aren't contained by
   the statement itself. for exaxmple, "int a" would be extracted from
   "<<< 4 => int a >>>" but not from "for(0 => int a;;);". A copy of the statement
   with used declarations removed is also returned. For example: "<<< 4 => int a >>>"
   becomes "<<< 4 => a >>>". *)
let rec extract_stmt_cntxt stmt =
  match stmt with
    ExprStatement e -> let (c, e') = extract_expr_cntxt e in (c, ExprStatement e')
  | Print args ->
      let (c, args') = extract_list_cntxt args extract_expr_cntxt in
      (c, Print args')
  | _ -> (Context.empty, stmt)

(* returns the type which best covers t1 and t2 *)
let rec promote_type t1 t2 =
  let fail () =
    raise (Compile_error ("incompatible types " ^ (string_of_type t1) ^ " and " ^ (string_of_type t2)))
  in
  if t1 = t2 then t1 else
  match (t1, t2) with
    (ArrayType (_, _), _) | (_, ArrayType (_, _)) -> fail ()
  | (RefType t1', RefType t2') -> RefType(promote_type t1' t2')
  | (RefType _, _) | (_, RefType _) -> fail ()
  | (StringType, _) | (_, StringType) -> StringType
  | (FloatType, _) | (_, FloatType) -> FloatType
  | (IntType, _) | (_, IntType) -> IntType
  | _ -> fail ()

let cast a b =
  if a = b then
    []
  else
    match (a, b) with
      (IntType, FloatType)
    | (IntType, BoolType)
    | (FloatType, IntType)
    | (FloatType, BoolType)
    | (BoolType, IntType)
    | (BoolType, FloatType) -> [ICast b]
    | _ -> raise (Compile_error ("cannot convert from " ^ (string_of_type a) ^ " to " ^ (string_of_type b)))

(* TODO: add cast before assignment *)
(* TODO: check variables for existence *)
(* when an expression finishes executing, there should be one more value on the stack *)
let rec compile_expr cntxt expr =
  match expr with
    Int i -> IntType, [IPush (IntData i)]
  | Float f -> FloatType, [IPush (FloatData f)]
  | Bool b -> BoolType, [IPush (BoolData b)]
  | String s -> StringType, [IPush (StringData s)]
  | Var name ->
      if is_builtin name then
        (builtin_type name, [IPushVar name])
      else
        let t = (try Context.find name cntxt with Not_found -> undeclared name) in
        (t, [IPushVar name])
  | Array exprs -> raise (Not_implemented "cannot compile array expressions")
  | Comma exprs -> List.fold_left (fun (t, instrs) e -> let (t, i) = compile_expr cntxt e in (t, instrs @ [IDiscard] @ i)) (BoolType, [IPush (BoolData false)]) exprs (* TODO: doity *)
  | UnaryExpr (PreInc as op, Var v)
  | UnaryExpr (PostInc as op, Var v)
  | UnaryExpr (PreDec as op, Var v)
  | UnaryExpr (PostDec as op, Var v) ->
      let t = (try Context.find v cntxt with Not_found -> undeclared v) in
      if t = IntType then
        let instr =
          (match op with
             PreInc -> IPreInc v
           | PostInc -> IPostInc v
           | PreDec -> IPreDec v
           | PostDec -> IPostDec v
           | _ -> raise (Compile_error "cannot happen"))
        in (t, [instr])
      else
        raise (Compile_error ("cannot use ++ on " ^ (string_of_type t)))
  | UnaryExpr (PreInc, _) | UnaryExpr (PostInc, _) -> raise (Compile_error ("cannot increment this type of expression"))
  | UnaryExpr (PreDec, _) | UnaryExpr (PostDec, _) -> raise (Compile_error ("cannot decrement this type of expression"))
  | UnaryExpr (op, e1) -> raise (Not_implemented "cannot compile this unary expression")
  | BinaryExpr (Chuck, e, Var "now") ->
      let t, i = compile_expr cntxt e in
      (match t with
         DurType -> (TimeType, i @ [IYield])
       | _ -> raise (Compile_error ("cannot chuck " ^ (string_of_type t) ^ " to now")))
  | BinaryExpr (Chuck, e, Var v) -> let t, i = compile_expr cntxt e in (t, i @ [(* TODO: cast *)] @ [IAssign v])
  | BinaryExpr (op, e1, e2) ->
      let (t1, i1) = compile_expr cntxt e1 in
      let (t2, i2) = compile_expr cntxt e2 in
      let instrs_n_casts t = i1 @ (cast t1 t) @ i2 @ (cast t2 t) in
      (match (op, t1, t2) with
         (Plus, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IAdd])
       | (Plus, FloatType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IAdd])
       | (Plus, IntType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IAdd])
       | (Plus, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IAdd])
       | (Minus, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [ISubtract])
       | (Minus, FloatType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [ISubtract])
       | (Minus, IntType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [ISubtract])
       | (Minus, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [ISubtract])
       | (Multiply, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IMultiply])
       | (Multiply, FloatType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IMultiply])
       | (Multiply, IntType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IMultiply])
       | (Multiply, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IMultiply])
       | (Divide, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IDivide])
       | (Divide, FloatType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IDivide])
       | (Divide, IntType, FloatType) -> (FloatType, (instrs_n_casts FloatType) @ [IDivide])
       | (Divide, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IDivide])
       | (LessThan, IntType, IntType) -> (BoolType, (instrs_n_casts IntType) @ [ILessThan])
       | (LessThan, FloatType, FloatType) -> (BoolType, (instrs_n_casts FloatType) @ [ILessThan])
       | (LessThan, IntType, FloatType) -> (BoolType, (instrs_n_casts FloatType) @ [ILessThan])
       | (LessThan, FloatType, IntType) -> (BoolType, (instrs_n_casts FloatType) @ [ILessThan])
       | (GreaterThan, IntType, IntType) -> (BoolType, (instrs_n_casts IntType) @ [IGreaterThan])
       | (GreaterThan, FloatType, FloatType) -> (BoolType, (instrs_n_casts FloatType) @ [IGreaterThan])
       | (GreaterThan, IntType, FloatType) -> (BoolType, (instrs_n_casts FloatType) @ [IGreaterThan])
       | (GreaterThan, FloatType, IntType) -> (BoolType, (instrs_n_casts FloatType) @ [IGreaterThan])
       | _ -> raise (Not_implemented "cannot compile that binary expression")
       )
  | Member (e1, mem) -> raise (Not_implemented "cannot compile member expressions")
  | FunCall (e1, args) -> raise (Not_implemented "cannot compile function calls")
  | Cast (e1, t) -> raise (Not_implemented "cannot compile casts")
  | Spork e1 -> raise (Not_implemented "cannot compile sporks")
  | Trinary (cond, e1, e2) ->
      let (tc, ic) = compile_expr cntxt cond in
      let (t1, i1) = compile_expr cntxt e1 in
      let (t2, i2) = compile_expr cntxt e2 in
      let return_type = promote_type t1 t2 in
      (return_type, ic @ (cast tc BoolType) @ [IBranch (i1 @ (cast t1 return_type), i2 @ (cast t2 return_type))])
  | Subscript (e1, e2) -> raise (Not_implemented "cannot compile subscription")
  | Time (e1, e2) ->
      let (t1, i1) = compile_expr cntxt e1 in
      let (t2, i2) = compile_expr cntxt e2 in
      (match t1, t2 with
         (IntType, DurType) -> DurType, i1 @ i2 @ [IMultiply]
       | (FloatType, DurType) -> DurType, i1 @ i2 @ [IMultiply]
       | _ -> raise (Compile_error ("invalid time expression: " ^ (string_of_type t1) ^ "::" ^ (string_of_type t2))))
  | Declaration decls -> raise (Compiler_error "declaration wasn't extracted earlier")

(* when a statement finishes executing, the stack should be how it was before *)
let rec compile_stmt parent_cntxt local_cntxt stmt =
  let (subcntxt, stmt') = extract_stmt_cntxt stmt in
  let this_context = combine_cntxts false local_cntxt subcntxt in
  let cntxt = combine_cntxts true parent_cntxt this_context in
  let instrs =
    match stmt' with
      NullStatement -> []
    | ExprStatement e -> let (t, i) = compile_expr cntxt e in i @ [IDiscard]
    | If (cond, s1, s2) ->
        let (cond_cntxt, cond) = extract_expr_cntxt cond in
        let cntxt = combine_cntxts true cntxt cond_cntxt in
        let (tc, ic) = compile_expr cntxt cond in
        let (iftrue_cntxt, iftrue_instrs) = compile_stmts cntxt s1 in
        let (iffalse_cntxt, iffalse_instrs) = compile_stmts cntxt s2 in
        [IPushEnv cond_cntxt]
          @ ic
          @ (cast tc BoolType)
          @ [IBranch ([IPushEnv iftrue_cntxt] @ iftrue_instrs,
                      [IPushEnv iffalse_cntxt] @ iffalse_instrs)]
          @ [IPopEnv]
    | While (cond, stmts) ->
        let (cond_cntxt, cond) = extract_expr_cntxt cond in
        let cntxt = combine_cntxts true cntxt cond_cntxt in
        let (tc, ic) = compile_expr cntxt cond in
        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in
        [IPushEnv (combine_cntxts false cond_cntxt body_cntxt)]
          @ ic
          @ (cast tc BoolType)
          @ [IWhile (body_instrs @ ic @ (cast tc BoolType))]
          @ [IPopEnv]
    | Repeat (cond, stmts) ->
        let (cond_cntxt, cond) = extract_expr_cntxt cond in
        let cntxt = combine_cntxts true cntxt cond_cntxt in
        let (tc, ic) = compile_expr cntxt cond in
        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in
        if tc = IntType then
          [IPushEnv (combine_cntxts false cond_cntxt body_cntxt)]
            @ ic
            @ [IRepeat body_instrs]
            @ [IPopEnv]
        else
          raise (Compile_error ("argument in repeat must be int, not " ^ (string_of_type tc)))
    | For (init, cond, incr, stmts) ->
        let (init_cntxt, init) = extract_expr_cntxt init in
        let cntxt = combine_cntxts true cntxt init_cntxt in
        let (_, init_instrs) = compile_expr cntxt init in

        let (cond_cntxt, cond) = extract_expr_cntxt cond in
        let cntxt = combine_cntxts false cntxt cond_cntxt in
        let (cond_type, cond_instrs) = compile_expr cntxt cond in

        let (incr_cntxt, incr) = extract_expr_cntxt incr in
        let cntxt = combine_cntxts false cntxt incr_cntxt in
        let (_, incr_instrs) = compile_expr cntxt incr in

        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in

        [IPushEnv (combine_cntxt_list false [init_cntxt; cond_cntxt; incr_cntxt; body_cntxt])]
          @ init_instrs @ [IDiscard]
          @ cond_instrs
          @ (cast cond_type BoolType)
          @ [IWhile (body_instrs @ incr_instrs @ [IDiscard] @ cond_instrs @ (cast cond_type BoolType))]
          @ [IPopEnv]
    | Break -> [IBreak]
    | Print args ->
        let instrs = List.fold_left (fun instrs e -> let (t, i) = compile_expr cntxt e in instrs @ i) [] args in
        instrs @ [IPrint (List.length args)]
    | _ -> raise (Not_implemented "cannot compile this type of statement")
  in
  (subcntxt, instrs)
and
compile_stmts parent_cntxt stmts =
  let local_cntxt = ref Context.empty in
  let compile_stmt instrs stmt =
    let (cntxt', instrs') = compile_stmt parent_cntxt !local_cntxt stmt in
    local_cntxt := combine_cntxts false !local_cntxt cntxt';
    instrs @ instrs'
  in
  (!local_cntxt, List.fold_left compile_stmt [] stmts)

(* returns a pair: context of stmts * instructions of stmts *)
let compile cntxt (fns, classes, stmts) : shred_template =
  let cntxt, instrs = compile_stmts Context.empty stmts in
  (cntxt, [], instrs)