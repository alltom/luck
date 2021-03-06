
exception Machine_error of string
exception Not_implemented of string
let error msg = raise (Machine_error msg)

type time = float (* duration or time in samples *)

type typ =
  VoidType
| ArrayType of array_dimension * typ
| RefType of typ
| IntType | FloatType | BoolType | StringType | DurType | TimeType
and array_dimension = Dynamic | Static of int

type data =
  ArrayData of data array
| RefData of data ref
| IntData of int
| BoolData of bool
| FloatData of float
| StringData of string
| DurData of float
| TimeData of float

(* map from string names to data types;
   used as template for creating environments *)
module Context = Map.Make(String)
type context = typ Context.t

type instruction =
  IPushEnv of context | IPopEnv
| IPush of data | IPushVar of string | IDiscard
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of instruction list * instruction list (* if true body, if false body *)
| IWhile of instruction list (* body instructions *)
| IRepeat of instruction list (* body instructions *)
| IBreak (* pops a WhileFrame and an environment *)
| IReturn
| IPrint of int (* number of things to print (consumes) *)
| ICast of typ
| IAdd | ISubtract | IMultiply | IDivide
| ILessThan | IGreaterThan
| IPreInc of string | IPostInc of string
| IPreDec of string | IPostDec of string
| IYield

(* return type * argument types * instructions *)
type func = typ * typ list * instruction list

(* map from string names to functions *)
module FunMap = Map.Make(String)
type funmap = func FunMap.t

(* TODO *)
type clas = unit

type shred_template = context * funmap * instruction list
type function_template = typ list * instruction list
type class_template = instruction list * function_template list * context

module Env =
  struct
    module StringMap = Map.Make(String)
    type member = data ref
    type environment = func StringMap.t * member StringMap.t
    let empty : environment = StringMap.empty, StringMap.empty
    let add_func name fn = function funcs, mems -> (StringMap.add name fn funcs, mems)
    let add_mem name v = function funcs, mems -> (funcs, StringMap.add name v mems)
    let find_func name = function funcs, _ -> StringMap.find name funcs
    let find_mem name = function _, mems -> StringMap.find name mems
  end


(* instruction lists are popped and pushed in blocks called frames
   as functions are called, loops entered, etc *)
type frame_type =
  TopLevelFrame
| FunCallFrame of typ
| WhileFrame of instruction list (* body of loop *)
| RepeatFrame of int * instruction list
type stack = data list
type frame = Frame of frame_type * instruction list * stack * Env.environment list * frame | NilFrame

(* STRING CONVERSIONS *)

let rec string_of_type = function
  VoidType -> "void"
| ArrayType (sz, t') -> (string_of_type t') ^ "[]"
| RefType t' -> (string_of_type t') ^ "@"
| IntType -> "int"
| BoolType -> "bool"
| FloatType -> "float"
| StringType -> "string"
| DurType -> "dur"
| TimeType -> "time"

let rec type_of_data = function
  ArrayData elems -> raise (Not_implemented "type_of_data for arrays")
| RefData d' -> RefType (type_of_data !d')
| IntData _ -> IntType
| BoolData _ -> BoolType
| FloatData _ -> FloatType
| StringData _ -> StringType
| DurData _ -> DurType
| TimeData _ -> TimeType

let strings_of_cntxt cntxt =
  Context.fold (fun name (t, d) lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

let string_of_data = function
  ArrayData a -> "array"
| RefData r -> "ref"
| IntData i -> string_of_int i
| BoolData b -> string_of_bool b
| FloatData f -> string_of_float f
| StringData s -> s
| DurData s -> (string_of_float s)
| TimeData s -> (string_of_float s)

let rec string_of_instruction = function
  IPushEnv cntxt -> "push env"
| IPopEnv -> "pop env"
| IPush d -> "push value " ^ (string_of_data d)
| IDiscard -> "discard"
| IPushVar v -> "push var " ^ v
| IAssign s -> "assign " ^ s
| IBranch (f1, f2) -> "if (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") else (" ^ (String.concat "; " (List.map string_of_instruction f2)) ^ ")"
| IWhile body -> "while (...) { " ^ (String.concat "; " (List.map string_of_instruction body)) ^ " }"
| IRepeat body -> "repeat (...) { " ^ (String.concat "; " (List.map string_of_instruction body)) ^ " }"
| IBreak -> "break"
| IReturn -> "return"
| IPrint i -> "print " ^ (string_of_int i)
| ICast t -> "cast -> " ^ (string_of_type t)
| IAdd -> "add"
| ISubtract -> "subtract"
| IMultiply -> "multiply"
| IDivide -> "divide"
| ILessThan -> "less than"
| IGreaterThan -> "greater than"
| IPreInc v -> "++" ^ v | IPostInc v -> v ^ "++"
| IPreDec v -> "--" ^ v | IPostDec v -> v ^ "--"
| IYield -> "yield"

(* UTILITY *)

(* fold, but with an int instead of a list *)
let rec nfold f memo n =
  if n > 0 then nfold f (f memo) (n-1) else memo

(* pops one item from a stack, returning the item and the stack *)
let pop = function
  d :: stck -> (d, stck)
| _ -> error "stack underflow"

(* pushes an item onto a stack *)
let push d stck = d :: stck

(* pops a bool *)
let pop_bool stck =
  match pop stck with
    (BoolData b, stck') -> (b, stck')
  | _ -> error "invalid stack: expected bool"

(* pops an int *)
let pop_int stck =
  match pop stck with
    (IntData i, stck') -> (i, stck')
  | _ -> error "invalid stack: expected int"

(* pops a dur *)
let pop_dur stck =
  match pop stck with
    (DurData d, stck') -> (d, stck')
  | _ -> error "invalid stack: expected dur"

(* pops n items off the stack *)
let npop n stck =
  nfold (fun (popped, stck) -> let (d, stck') = pop stck in (d :: popped, stck')) ([], stck) n

(* EXECUTION *)

let finished = function ([], [], _) -> true | _ -> false

(* prints count items from the stack, popping them *)
let print count stck =
  let (args, stck) = npop count stck in
  print_endline (String.concat " " (List.map string_of_data args));
  stck

(* searches an environment stack for a variable *)
let rec find_mem envs var =
  match envs with
    env :: rest -> (try Env.find_mem var env with Not_found -> find_mem rest var)
  | [] -> error ("variable " ^ var ^ " does not exist")

(* searches built-ins and the given environment stack for a variable *)
let rec find_var envs var =
  match var with
    "samp" -> ref (DurData 1.)
  | _ -> find_mem envs var

(* instantiates an environment with variables from a context *)
let inst_context cntxt =
  let inst_type name = function
      IntType -> IntData 0
    | FloatType -> FloatData 0.0
    | BoolType -> BoolData false
    | StringType -> StringData ""
    | DurType -> DurData 0.0
    | t -> error ("cannot instantiate type " ^ (string_of_type t))
  in
  Context.fold (fun name t env -> Env.add_mem name (ref (inst_type name t)) env) cntxt Env.empty

(* data should already be casted so types match (see compile.ml) *)
let exec_binop instr stck =
  let (b, stck) = pop stck in (* pop in reverse order! *)
  let (a, stck) = pop stck in
  let result =
    match instr, a, b with
      IAdd,         IntData a,   IntData b   -> IntData   (a + b)
    | IAdd,         FloatData a, FloatData b -> FloatData (a +. b)
    | ISubtract,    IntData a,   IntData b   -> IntData   (a - b)
    | ISubtract,    FloatData a, FloatData b -> FloatData (a -. b)
    | IMultiply,    IntData a,   IntData b   -> IntData   (a * b)
    | IMultiply,    FloatData a, FloatData b -> FloatData (a *. b)
    | IMultiply,    IntData a,   DurData b   -> DurData   ((float_of_int a) *. b)
    | IMultiply,    FloatData a, DurData b   -> DurData   (a *. b)
    | IDivide,      IntData a,   IntData b   -> IntData   (a / b)
    | IDivide,      FloatData a, FloatData b -> FloatData (a /. b)
    | ILessThan,    IntData a,   IntData b   -> BoolData  (a < b)
    | ILessThan,    FloatData a, FloatData b -> BoolData  (a < b)
    | IGreaterThan, IntData a,   IntData b   -> BoolData  (a > b)
    | IGreaterThan, FloatData a, FloatData b -> BoolData  (a > b)
    | _, _, _ -> error "cannot execute this binary expression"
  in
  result :: stck

(* executes a single instruction *)
let exec frame = match frame with
  NilFrame
| Frame (_, [], _, _, _) ->
  NilFrame

| Frame(typ, (IPushEnv cntxt) :: instrs, stack, envs, parent) ->
  Frame(typ, instrs, stack, (inst_context cntxt) :: envs, parent)

| Frame(typ, IPopEnv :: instrs, stack, _ :: envs, parent) ->
  Frame(typ, instrs, stack, envs, parent)
| Frame(typ, IPopEnv :: instrs, stack, [], parent) ->
  error "env underflow"

| Frame(typ, (IPush d) :: instrs, stack, envs, parent) ->
  Frame(typ, instrs, d :: stack, envs, parent)

| Frame(typ, (IPrint count) :: instrs, stack, envs, parent) ->
  Frame(typ, instrs, (print count stack), envs, parent)

| Frame(typ, (IPushVar v) :: instrs, stack, envs, parent) ->
  Frame(typ, instrs, !(find_var envs v) :: stack, envs, parent)

| Frame(typ, (IAssign var) :: instrs, stack, envs, parent) ->
  let (v, stack) = pop stack in
  (find_var envs var) := v;
  Frame(typ, instrs, v :: stack, envs, parent)

| Frame(typ, IDiscard :: instrs, _ :: stack, envs, parent) ->
  Frame(typ, instrs, stack, envs, parent)
| Frame(typ, IDiscard :: instrs, [], envs, parent) ->
  error "stack underflow"

| Frame(typ, (IAdd as op) :: instrs, stack, envs, parent)
| Frame(typ, (ISubtract as op) :: instrs, stack, envs, parent)
| Frame(typ, (IMultiply as op) :: instrs, stack, envs, parent)
| Frame(typ, (IDivide as op) :: instrs, stack, envs, parent)
| Frame(typ, (ILessThan as op) :: instrs, stack, envs, parent)
| Frame(typ, (IGreaterThan as op) :: instrs, stack, envs, parent) ->
  Frame(typ, instrs, exec_binop op stack, envs, parent)

| Frame(typ, (IPreInc v as op) :: instrs, stack, envs, parent)
| Frame(typ, (IPostInc v as op) :: instrs, stack, envs, parent)
| Frame(typ, (IPreDec v as op) :: instrs, stack, envs, parent)
| Frame(typ, (IPostDec v as op) :: instrs, stack, envs, parent) ->
  let slot = (find_mem envs v) in
  (match !slot with
     IntData i ->
       let newval, retval =
         (match op with
            IPreInc _ -> i+1, i+1
          | IPostInc _ -> i+1, i
          | IPreDec _ -> i-1, i-1
          | IPostDec _ -> i-1, i
          | _ -> error "cannot happen")
       in
       slot := IntData newval;
       Frame(typ, instrs, (IntData retval) :: stack, envs, parent)
   | _ -> error ("incr/decr applied to invalid data type, " ^ (string_of_type (type_of_data !slot))))

| Frame(typ, (ICast t) :: instrs, v :: stack, envs, parent) ->
  (match t, v with
     BoolType, IntData i -> Frame(typ, instrs, (BoolData (i != 0)) :: stack, envs, parent)
   | _ -> error ("cannot convert " ^ (string_of_type (type_of_data v)) ^ " to " ^ (string_of_type t)))

| Frame(typ, (IBranch (f1, f2)) :: instrs, stack, envs, parent) ->
    let (cond, stack) = pop_bool stack in
    if cond then
      Frame(typ, f1 @ instrs, stack, envs, parent)
    else
      Frame(typ, f2 @ instrs, stack, envs, parent)

| Frame(typ, (IWhile body_instrs) :: instrs, stack, envs, parent) ->
    let (cond, stack) = pop_bool stack in
    if cond then
      Frame(WhileFrame body_instrs, body_instrs, stack, envs, Frame(typ, instrs, stack, envs, parent))
    else
      Frame(typ, instrs, stack, envs, parent)

| Frame(typ, (IRepeat body_instrs) :: instrs, stack, envs, parent) ->
    let (times, stack) = pop_int stack in
    if times > 0 then
      Frame(RepeatFrame (times - 1, body_instrs), body_instrs, stack, envs, Frame(typ, instrs, stack, envs, parent))
    else
      Frame(typ, instrs, stack, envs, parent)

| Frame(typ, IBreak :: instrs, stack, envs, parent) ->
  parent

(* BEGIN: error cases *)

(* IYield should be caught in run_til_yield, but one got through *)
| Frame(typ, IYield :: instrs, stack, envs, parent) ->
  error "run_til_yield passed IYield to exec"

(* an unknown instruction was encountered *)
| Frame (typ, instr :: instrs, stack, envs, parent) ->
  print_endline ("ending on unknown instruction: " ^ (string_of_instruction instr));
  NilFrame


(* executes instructions in the given environments until it yields or finishes *)
(* returns the number of samples yielded and the new execution state *)
let rec run_til_yield frame =
  match frame with
    NilFrame -> None
  | Frame (typ, IYield :: instrs, stack, envs, parent) ->
      let d, stack = pop_dur stack in
      Some (d, Frame(typ, instrs, (TimeData 0.0 (* TODO: now *)) :: stack, envs, parent))
  | Frame (TopLevelFrame, [], stack, envs, parent) -> None
  | Frame (FunCallFrame VoidType, [], stack, envs, parent) -> run_til_yield parent
  | Frame (FunCallFrame _, IReturn :: _, retval :: _, _, Frame (typ', instrs', stack', envs', parent')) ->
      run_til_yield (Frame (typ', instrs', retval :: stack', envs', parent'))
  | Frame (FunCallFrame _, [], _, _, _) ->
      error "reached end of non-void function"
  | Frame (WhileFrame body, [], stack, envs, parent) ->
      let (cond, stack) = pop_bool stack in
      if cond then
        run_til_yield (Frame (WhileFrame body, body, stack, envs, parent))
      else
        run_til_yield parent
  | Frame (RepeatFrame (times, body), [], stack, envs, parent) ->
      if times > 0 then
        run_til_yield (Frame (RepeatFrame (times - 1, body), body, stack, envs, parent))
      else
        run_til_yield parent
  | Frame (typ, instrs, stack, envs, parent) ->
      run_til_yield (exec frame)
