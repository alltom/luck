
type typ =
  ArrayType of typ
| RefType of typ
| IntType | FloatType | BoolType | StringType

let rec string_of_type t =
  match t with
    ArrayType t' -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | BoolType -> "bool"
  | FloatType -> "float"
  | StringType -> "string"

module Context = Map.Make(String)

let strings_of_cntxt cntxt =
  Context.fold (fun name (t, d) lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

type data =
  ArrayData of data array
| RefData of data
| IntData of int
| BoolData of bool
| FloatData of float
| StringData of string

and instruction =
  IPushEnv of (typ Context.t) | IPopEnv
| IPush of data
| IDiscard
| IInit of string * data
| IPushVar of string
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of instruction list * instruction list (* if true body, if false body *)
| IWhile of instruction list * (typ Context.t) * instruction list (* condition, body context, body instructions *)
| IBreak (* pops a LoopFrame and an environment *)
| IPrint of int (* number of things to print (consumes) *)
| ICast of typ * typ
| IAdd | ISubtract | IMultiply | IDivide
| ILessThan | IGreaterThan

and frame_type =
  Frame
| LoopFrame of instruction list (* body of loop *)

and environment = (string * data ref) list

and stack = data list

exception Machine_error of string

let string_of_data = function
  ArrayData a -> "array"
| RefData r -> "ref"
| IntData i -> string_of_int i
| BoolData b -> string_of_bool b
| FloatData f -> string_of_float f
| StringData s -> s

let rec string_of_instruction = function
  IPushEnv cntxt -> "push env"
| IPopEnv -> "pop env"
| IPush d -> "push value " ^ (string_of_data d)
| IDiscard -> "discard"
| IInit (v, d) -> "init " ^ v ^ " = " ^ (string_of_data d)
| IPushVar v -> "push var " ^ v
| IAssign s -> "assign " ^ s
| IBranch (f1, f2) -> "if (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") (" ^ (String.concat "; " (List.map string_of_instruction f2)) ^ ")"
| IWhile (f1, cntxt, f2) -> "while (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") { " ^ (String.concat "; " (List.map string_of_instruction f2)) ^ " }"
| IBreak -> "break"
| IPrint i -> "print " ^ (string_of_int i)
| ICast (t1, t2) -> "cast " ^ (string_of_type t1) ^ " -> " ^ (string_of_type t2)
| IAdd -> "add"
| ISubtract -> "subtract"
| IMultiply -> "multiply"
| IDivide -> "divide"
| ILessThan -> "less than"
| IGreaterThan -> "greater than"
  
let error msg =
  raise (Machine_error msg)

let rec nfold f memo n =
  if n > 0 then nfold f (f memo) (n-1) else memo

let pop = function
  d :: stck -> (d, stck)
| _ -> error "stack underflow"

let push d stck = d :: stck

let pop_bool stck =
  match pop stck with
    (BoolData b, stck') -> (b, stck')
  | _ -> error "invalid stack: expected bool"

let pop_int stck =
  match pop stck with
    (IntData i, stck') -> (i, stck')
  | _ -> error "invalid stack: expected int"

let npop n stck =
  nfold (fun (popped, stck) -> let (d, stck') = pop stck in (d :: popped, stck')) ([], stck) n

let print count stck =
  let (args, stck) = npop count stck in
  print_endline (String.concat " " (List.map string_of_data args));
  stck

let rec lookup envs var =
  match envs with
    ((name, d) :: env') :: envs' -> (if name = var then d else lookup (env' :: envs') var)
  | [] :: envs' -> lookup envs' var
  | [] -> error ("variable " ^ var ^ " does not exist")

let env_insert var v env = (var, ref v) :: env
let assign envs var v = (lookup envs var) := v

let inst_context cntxt =
  let inst_type name = function
      IntType -> IntData 0
    | FloatType -> FloatData 0.0
    | BoolType -> BoolData false
    | StringType -> StringData ""
    | _ -> error "cannot instantiate that type yet"
  in
  Context.fold (fun name t env -> env_insert name (inst_type name t) env) cntxt []

(* data should already be casted (see compile.ml) *)
let exec_binop instr stck =
  let (b, stck) = pop stck in (* pop in reverse order! *)
  let (a, stck) = pop stck in
  let result =
    match instr, a, b with
      IAdd, IntData a, IntData b -> IntData (a + b)
    | IAdd, FloatData a, FloatData b -> FloatData (a +. b)
    | ISubtract, IntData a, IntData b -> IntData (a - b)
    | ISubtract, FloatData a, FloatData b -> FloatData (a -. b)
    | IMultiply, IntData a, IntData b -> IntData (a * b)
    | IMultiply, FloatData a, FloatData b -> FloatData (a *. b)
    | IDivide, IntData a, IntData b -> IntData (a / b)
    | IDivide, FloatData a, FloatData b -> FloatData (a /. b)
    | ILessThan, IntData a, IntData b -> BoolData (a < b)
    | ILessThan, FloatData a, FloatData b -> BoolData (a < b)
    | IGreaterThan, IntData a, IntData b -> BoolData (a > b)
    | IGreaterThan, FloatData a, FloatData b -> BoolData (a > b)
    | _, _, _ -> error "cannot execute this binary expression"
  in
  result :: stck

let exec instr frms stck envs =
  match instr with
    IPushEnv cntxt -> (frms, stck, (inst_context cntxt) :: envs)
  | IPopEnv ->
      (match envs with
         _ :: envs -> (frms, stck, envs)
       | _ -> error "cannot pop environment")
  | IPush d -> (frms, d :: stck, envs)
  | IPushVar var -> (frms, !(lookup envs var) :: stck, envs)
  | IDiscard -> let (v, stck) = pop stck in (frms, stck, envs)
  | IInit (v, d) -> (match envs with env::rest -> (frms, stck, (env_insert v d env) :: rest) | [] -> error "weird environment")
  | IAssign var -> let (v, stck) = pop stck in assign envs var v; (frms, v :: stck, envs)
  | IBranch (f1, f2) ->
      let (cond, stck) = pop_bool stck in
      (match frms with
         (ft, instrs) :: frms -> ((ft, (if cond then f1 else f2) @ instrs) :: frms, stck, envs)
       | _ -> error "in branch, expecting a frame")
  | IWhile (condframe, body_cntxt, body_frame) -> (* TODO: push body context first time into while *)
      let (cond, stck) = pop_bool stck in
      if cond then
        ((LoopFrame(body_frame @ condframe), body_frame @ condframe) :: frms, stck, envs)
      else
        (frms, stck, envs)
  | IBreak ->
      (match (frms, envs) with
         ((LoopFrame _, _) :: frms, _ :: envs) -> (frms, stck, envs)
       | (_ :: frms, _ :: envs) -> error "cannot break out of a non-loop frame"
       | _ -> error "missing a frame or environment to pop")
  | IPrint count -> (frms, (print count stck), envs)
  | ICast (t1, t2) ->
      let (v, stck) = pop stck in
      (match t1, t2, v with
         IntType, BoolType, IntData i -> (frms, push (BoolData (i != 0)) stck, envs)
       | _ -> error ("cannot convert " ^ (string_of_type t1) ^ " to " ^ (string_of_type t2)))
  | IAdd | ISubtract | IMultiply | IDivide | ILessThan | IGreaterThan -> (frms, exec_binop instr stck, envs)

let run instrs env =
  let rec loop (frms, stck, envs) =
    match (frms, stck, envs) with
      ([], [v], _) -> Some v
    | ([], [], _) -> None
    | ((ft, i::is) :: frms, stck, envs) -> loop (exec i ((ft, is)::frms) stck envs)
    | ((Frame, []) :: frms, stck, envs) -> loop (frms, stck, envs)
    | ((LoopFrame body, []) :: frms, stck, envs) ->
        let (cond, stck) = pop_bool stck in
        if cond then
          loop ((LoopFrame body, body) :: frms, stck, envs)
        else
          loop (frms, stck, envs)
    | _ -> error "invalid machine state"
  in
  loop ([Frame, instrs], [], [env])
