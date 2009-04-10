
exception Machine_error of string
let error msg =
  raise (Machine_error msg)

type typ =
  ArrayType of array_dimension * typ
| RefType of typ
| IntType | FloatType | BoolType | StringType
and array_dimension = Dynamic | Static of int

type data =
  ArrayData of data array
| RefData of data ref
| IntData of int
| BoolData of bool
| FloatData of float
| StringData of string

(* map from string names to data types;
   used as template for creating environments *)
module Context = Map.Make(String)
type context = typ Context.t

type instruction =
  IPushEnv of context | IPopEnv
| IPush of data
| IDiscard
| IPushVar of string
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of instruction list * instruction list (* if true body, if false body *)
| IWhile of instruction list (* body instructions *)
| IBreak (* pops a LoopFrame and an environment *)
| IPrint of int (* number of things to print (consumes) *)
| ICast of typ * typ
| IAdd | ISubtract | IMultiply | IDivide
| ILessThan | IGreaterThan

type time = float
type func = typ * typ list * instruction list
type shred_template = context * func list * instruction list

(* TODO: classes *)
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
type frame_type = Frame | LoopFrame of instruction list (* body of loop *)
type frame = frame_type * instruction list
type env_stack = (Env.environment list) list
type stack = data list
type execution_state = frame list * stack * env_stack

module Shred =
  struct
    type shred = time * execution_state
    let shred now state = (now, state)
    let now shred = let (now, _) = shred in now
    let state shred = let (_, state) = shred in state
  end

(* STRING CONVERSIONS *)

let rec string_of_type t =
  match t with
    ArrayType (sz, t') -> (string_of_type t') ^ "[]"
  | RefType t' -> (string_of_type t') ^ "@"
  | IntType -> "int"
  | BoolType -> "bool"
  | FloatType -> "float"
  | StringType -> "string"

let strings_of_cntxt cntxt =
  Context.fold (fun name (t, d) lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

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
| IPushVar v -> "push var " ^ v
| IAssign s -> "assign " ^ s
| IBranch (f1, f2) -> "if (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") (" ^ (String.concat "; " (List.map string_of_instruction f2)) ^ ")"
| IWhile body -> "while (...) { " ^ (String.concat "; " (List.map string_of_instruction body)) ^ " }"
| IBreak -> "break"
| IPrint i -> "print " ^ (string_of_int i)
| ICast (t1, t2) -> "cast " ^ (string_of_type t1) ^ " -> " ^ (string_of_type t2)
| IAdd -> "add"
| ISubtract -> "subtract"
| IMultiply -> "multiply"
| IDivide -> "divide"
| ILessThan -> "less than"
| IGreaterThan -> "greater than"

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

(* pops an item, checking that it's a bool *)
let pop_bool stck =
  match pop stck with
    (BoolData b, stck') -> (b, stck')
  | _ -> error "invalid stack: expected bool"

(* pops n items off the stack *)
let npop n stck =
  nfold (fun (popped, stck) -> let (d, stck') = pop stck in (d :: popped, stck')) ([], stck) n

let push_env env = function
  envs :: rest -> (env :: envs) :: rest
| [] -> [[env]]

let first_env_list = function
  envs :: rest -> envs
| [] -> error "expected an environment"

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

(* instantiates an environment with variables from a context *)
let inst_context cntxt =
  let inst_type name = function
      IntType -> IntData 0
    | FloatType -> FloatData 0.0
    | BoolType -> BoolData false
    | StringType -> StringData ""
    | t -> error ("cannot instantiate type " ^ (string_of_type t))
  in
  Context.fold (fun name t env -> Env.add_mem name (ref (inst_type name t)) env) cntxt Env.empty

(* TODO: global env *)
let instantiate_shred env (shred_templ : shred_template) : execution_state =
  let cntxt, funcs, instrs = shred_templ in
  ([(Frame, instrs)], [], [(inst_context cntxt) :: [env]])

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
let exec instr (frms : frame list) (stck : stack) (envs : env_stack) =
  match instr with
    IPushEnv cntxt -> (frms, stck, push_env (inst_context cntxt) envs)
  | IPopEnv ->
      (match envs with
         _ :: envs -> (frms, stck, envs)
       | _ -> error "cannot pop environment")
  | IPush d -> (frms, d :: stck, envs)
  | IPushVar var -> (frms, !(find_mem (first_env_list envs) var) :: stck, envs)
  | IDiscard -> let (v, stck) = pop stck in (frms, stck, envs)
  | IAssign var -> let (v, stck) = pop stck in (find_mem (first_env_list envs) var) := v; (frms, v :: stck, envs)
  | IBranch (f1, f2) ->
      let (cond, stck) = pop_bool stck in
      (match frms with
         (ft, instrs) :: frms -> ((ft, (if cond then f1 else f2) @ instrs) :: frms, stck, envs)
       | _ -> error "in branch, expecting a frame")
  | IWhile body_frame ->
      let (cond, stck) = pop_bool stck in
      if cond then
        ((LoopFrame body_frame, body_frame) :: frms, stck, envs)
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

(* executes instructions in the given environments until it yields or finishes *)
(* returns the number of samples yielded and the new execution state *)
let rec run_til_yield (state : execution_state) =
  match state with
    ([], [], _) -> None
  | ((ft, i::is) :: frms, stck, envs) -> run_til_yield (exec i ((ft, is)::frms) stck envs)
  | ((Frame, []) :: frms, stck, envs) -> run_til_yield (frms, stck, envs)
  | ((LoopFrame body, []) :: frms, stck, envs) ->
      let (cond, stck) = pop_bool stck in
      if cond then
        run_til_yield ((LoopFrame body, body) :: frms, stck, envs)
      else
        run_til_yield (frms, stck, envs)
  | (frms, stck, envs) -> error ("invalid machine state: "
                                 ^ (string_of_int (List.length frms)) ^ " frames, "
                                 ^ (string_of_int (List.length stck)) ^ " items on stack, "
                                 ^ (string_of_int (List.length envs)) ^ " envs")

(* maintains a sense of "now" given a set of shreds *)
(* TODO: instead of putting a shred back in the queue, run it
         repeatedly until it is no longer the furthest behind *)
module VM =
  struct
    type vm = time * Shred.shred Priority_queue.queue
    let empty now = (now, Priority_queue.empty)
    let add vm shred = Priority_queue.insert vm (Shred.now shred) shred
    let next_shred vm = let _, shred, queue = Priority_queue.extract vm in (shred, queue)
    let rec run samples vm =
      let shred, vm = next_shred vm in
      match run_til_yield (Shred.state shred) with
        None -> run samples vm
      | Some(elapsed, state) ->
          let vm = add vm (Shred.shred ((Shred.now shred) +. elapsed) state) in
          if elapsed > samples then vm else run (samples -. elapsed) vm
  end
