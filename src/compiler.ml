
open Ast
open Interpreter
open Vm

exception Compile_error of string (* something is wrong with the input *)
exception Compiler_error of string (* something went wrong internally *)
exception Cant_happen
exception Not_implemented of string

let undeclared var = raise (Compile_error ("use of undeclared variable " ^ var))
let cant_happen () = raise Cant_happen

let builtin_context =
  List.fold_left
    (fun cntxt (name, typ) -> Context.add name typ cntxt)
    Context.empty
    [("now", TimeType); ("samp", DurType); ("second", DurType)]
let builtin_type name = Context.find name builtin_context
let is_builtin name = Context.mem name builtin_context
let raise_if_reserved name =
  if is_builtin name then
    raise (Compile_error ("redeclaration of reserved variable " ^ name))
  else
    name

(* checks given context and built-ins for the type of a variable name *)
let get_type cntxt name =
  if is_builtin name then
    builtin_type name
  else
    Context.find name cntxt

let rec typ_of_asttype asttype =
  match asttype with
    Type("int", false, _, []) -> IntType
  | Type("float", false, _, []) -> FloatType
  | Type("string", false, _, []) -> StringType
  | Type("dur", false, _, []) -> DurType
  | _ -> raise (Not_implemented "cannot yet convert this data type")

let rec context_of_decls decls =
  let rec loop cntxt = function
      (name, t) :: rest ->
        loop (Context.add (raise_if_reserved name) (typ_of_asttype t) cntxt) rest
    | [] -> cntxt
  in loop Context.empty decls

let add_context overwrite cntxt_base cntxt_new =
  let add name entry c' =
    if not overwrite && (Context.mem name c') then
      raise (Compile_error ("redeclaration of " ^ name))
    else
      Context.add name entry c'
  in
  Context.fold add cntxt_new cntxt_base

(* expr_with_declarations => context * expr_without_declarations *)
let rec extract_expr_context expr =
  let recurse exprs f =
    let cntxt, exprs' = extract_list_context exprs in
    (cntxt, f exprs')
  in
  match expr with
    Array exprs -> recurse exprs (fun exprs' -> Array exprs')
  | Comma exprs -> recurse exprs (fun exprs' -> Comma exprs')
  | UnaryExpr (op, e1) -> recurse [e1] (function [e1'] -> UnaryExpr(op, e1') | _ -> cant_happen ())
  | BinaryExpr (op, e1, e2) -> recurse [e1; e2] (function [e1'; e2'] -> BinaryExpr(op, e1', e2') | _ -> cant_happen ())
  | Member (e1, m) -> recurse [e1] (function [e1'] -> Member(e1', m) | _ -> cant_happen ())
  | FunCall (e1, args) -> recurse ([e1] @ args) (function e1'::args' -> FunCall(e1', args') | _ -> cant_happen ())
  | Cast (e1, t) -> recurse [e1] (function [e1'] -> Cast(e1', t) | _ -> cant_happen ())
  | Spork e1 -> recurse [e1] (function [e1'] -> Spork e1' | _ -> cant_happen ())
  | Trinary (e1, e2, e3) -> recurse [e1; e2; e3] (function [e1'; e2'; e3'] -> Trinary(e1', e2', e3') | _ -> cant_happen ())
  | Subscript (e1, e2) -> recurse [e1; e2] (function [e1'; e2'] -> Subscript(e1', e2') | _ -> cant_happen ())
  | Time (e1, e2) -> recurse [e1; e2] (function [e1'; e2'] -> Time(e1', e2') | _ -> cant_happen ())
  | Int _ | Float _ | Bool _ | String _ | Var _ -> (Context.empty, expr)
  | Declaration decls ->
      let c = context_of_decls decls in
      (match decls with
         (name, _) :: [] -> (c, Var name)
       | _ -> (c, Array(List.map (fun (name, _) -> Var name) decls)))

(* builds a context from a list of expressions *)
and extract_list_context exprs =
  List.fold_left
    (fun (cntxt, fixed_exprs) expr ->
      let (cntxt', expr') = extract_expr_context expr in
      (add_context false cntxt cntxt', fixed_exprs @ [expr']))
    (Context.empty, [])
    exprs

let rec extract_stmt_context stmt =
  match stmt with
    ExprStatement e -> let (c, e') = extract_expr_context e in (c, ExprStatement e')
  | Print args -> let (c, args') = extract_list_context args in (c, Print args')
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
  | (TimeType, _) | (_, TimeType)
  | (DurType, _) | (_, DurType)
  | (BoolType, _) | (VoidType, _) -> fail ()

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

let rec compile_expr cntxt expr =
  match expr with
    Int i -> IntType, [IPush (IntData i)]
  | Float f -> FloatType, [IPush (FloatData f)]
  | Bool b -> BoolType, [IPush (BoolData b)]
  | String s -> StringType, [IPush (StringData s)]
  | Var name -> (try (get_type cntxt name, [IPushVar name]) with Not_found -> undeclared name)
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
  | BinaryExpr (Chuck, e, Var v) ->
      let t, i = compile_expr cntxt e in
      (t, i @ (cast t (get_type cntxt v)) @ [IAssign v])
  | BinaryExpr (op, e1, e2) ->
      let (t1, i1) = compile_expr cntxt e1 in
      let (t2, i2) = compile_expr cntxt e2 in
      let instrs_n_casts t = i1 @ (cast t1 t) @ i2 @ (cast t2 t) in
      (match (op, t1, t2) with
         (Plus, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IAdd])
       | (Plus, FloatType, FloatType)
       | (Plus, IntType, FloatType)
       | (Plus, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IAdd])
       
       | (Minus, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [ISubtract])
       | (Minus, FloatType, FloatType)
       | (Minus, IntType, FloatType)
       | (Minus, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [ISubtract])
       
       | (Multiply, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IMultiply])
       | (Multiply, FloatType, FloatType)
       | (Multiply, IntType, FloatType)
       | (Multiply, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IMultiply])
       
       | (Divide, IntType, IntType) -> (IntType, (instrs_n_casts IntType) @ [IDivide])
       | (Divide, FloatType, FloatType)
       | (Divide, IntType, FloatType)
       | (Divide, FloatType, IntType) -> (FloatType, (instrs_n_casts FloatType) @ [IDivide])
       
       | (LessThan, IntType, IntType) -> (BoolType, (instrs_n_casts IntType) @ [ILessThan])
       | (LessThan, FloatType, FloatType)
       | (LessThan, IntType, FloatType)
       | (LessThan, FloatType, IntType) -> (BoolType, (instrs_n_casts FloatType) @ [ILessThan])
       
       | (GreaterThan, IntType, IntType) -> (BoolType, (instrs_n_casts IntType) @ [IGreaterThan])
       | (GreaterThan, FloatType, FloatType)
       | (GreaterThan, IntType, FloatType)
       | (GreaterThan, FloatType, IntType) -> (BoolType, (instrs_n_casts FloatType) @ [IGreaterThan])
       
       | _ -> raise (Not_implemented "cannot compile that binary expression")
       )
  | Member (e1, mem) -> raise (Not_implemented "cannot compile member expressions")
  | FunCall (e1, args) -> raise (Not_implemented "cannot compile function calls")
  | Cast (e1, t) ->
      let t1, i1 = compile_expr cntxt e1 in
      let t2 = typ_of_asttype t in
      t2, i1 @ (cast t1 t2)
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
  let (subcntxt, stmt') = extract_stmt_context stmt in
  let this_context = add_context false local_cntxt subcntxt in
  let cntxt = add_context true parent_cntxt this_context in
  let instrs =
    match stmt' with
      NullStatement -> []
    | ExprStatement e -> let (t, i) = compile_expr cntxt e in i @ [IDiscard]
    | If (cond, s1, s2) ->
        let (cond_cntxt, cond) = extract_expr_context cond in
        let cntxt = add_context true cntxt cond_cntxt in
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
        let (cond_cntxt, cond) = extract_expr_context cond in
        let cntxt = add_context true cntxt cond_cntxt in
        let (tc, ic) = compile_expr cntxt cond in
        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in
        [IPushEnv (add_context false cond_cntxt body_cntxt)]
          @ ic
          @ (cast tc BoolType)
          @ [IWhile (body_instrs @ ic @ (cast tc BoolType))]
          @ [IPopEnv]
    | Repeat (cond, stmts) ->
        let (cond_cntxt, cond) = extract_expr_context cond in
        let cntxt = add_context true cntxt cond_cntxt in
        let (tc, ic) = compile_expr cntxt cond in
        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in
        if tc = IntType then
          [IPushEnv (add_context false cond_cntxt body_cntxt)]
            @ ic
            @ [IRepeat body_instrs]
            @ [IPopEnv]
        else
          raise (Compile_error ("argument in repeat must be int, not " ^ (string_of_type tc)))
    | For (init, cond, incr, stmts) ->
        let (init_cntxt, init) = extract_expr_context init in
        let cntxt = add_context true cntxt init_cntxt in
        let (_, init_instrs) = compile_expr cntxt init in

        let (cond_cntxt, cond) = extract_expr_context cond in
        let cntxt = add_context false cntxt cond_cntxt in
        let (cond_type, cond_instrs) = compile_expr cntxt cond in

        let (incr_cntxt, incr) = extract_expr_context incr in
        let cntxt = add_context false cntxt incr_cntxt in
        let (_, incr_instrs) = compile_expr cntxt incr in

        let (body_cntxt, body_instrs) = compile_stmts cntxt stmts in
        
        let new_cntxt =
          List.fold_left
            (fun c c' -> add_context false c c')
            Context.empty
            [init_cntxt; cond_cntxt; incr_cntxt; body_cntxt]
        in

        [IPushEnv new_cntxt]
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
    local_cntxt := add_context false !local_cntxt cntxt';
    instrs @ instrs'
  in
  (!local_cntxt, List.fold_left compile_stmt [] stmts)

(* returns a pair: context of stmts * instructions of stmts *)
let compile cntxt (fns, classes, stmts) : shred_template =
  let cntxt, instrs = compile_stmts Context.empty stmts in
  (cntxt, [], instrs)
