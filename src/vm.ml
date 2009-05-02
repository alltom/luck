
exception Machine_error of string
exception Not_implemented of string
let error msg = raise (Machine_error msg)

type time = float (* duration or time in samples *)

type typ =
  ArrayType of array_dimension * typ
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
let builtin_variables = [("samp", DurType); ("now", TimeType)]
let builtin_type name =
  let _, t = List.find (fun (name', t) -> name = name') builtin_variables in
  t
let builtin_cntxt =
  List.fold_left
    (fun cntxt (name, t) -> Context.add name t cntxt)
    Context.empty
    builtin_variables
let is_builtin name =
  List.exists (fun (name', _) -> name' = name) builtin_variables

type instruction =
  IPushEnv of context | IPopEnv
| IPush of data
| IDiscard
| IPushVar of string
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of instruction list * instruction list (* if true body, if false body *)
| IWhile of instruction list (* body instructions *)
| IRepeat of instruction list (* body instructions *)
| IBreak (* pops a WhileFrame and an environment *)
| IPrint of int (* number of things to print (consumes) *)
| ICast of typ
| IAdd | ISubtract | IMultiply | IDivide
| ILessThan | IGreaterThan
| IPreInc of string | IPostInc of string
| IPreDec of string | IPostDec of string
| IYield

type func = typ * typ list * instruction list
type shred_template = context * func list * instruction list

(* returns the instructions from a shred template *)
let shred_instructions (_, _, instrs) = instrs

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
type frame_type =
  Frame
| WhileFrame of instruction list (* body of loop *)
| RepeatFrame of int * instruction list
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
  | DurType -> "dur"
  | TimeType -> "time"

let rec type_of_data d =
  match d with
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
| IBranch (f1, f2) -> "if (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") (" ^ (String.concat "; " (List.map string_of_instruction f2)) ^ ")"
| IWhile body -> "while (...) { " ^ (String.concat "; " (List.map string_of_instruction body)) ^ " }"
| IRepeat body -> "repeat (...) { " ^ (String.concat "; " (List.map string_of_instruction body)) ^ " }"
| IBreak -> "break"
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
(* TODO: this should be moved to VM for builtins like "now" and "second" *)
let rec find_mem envs var =
  if is_builtin var then
    match var with
      "samp" -> ref (DurData 1.0)
    | _ -> error ("builtin " ^ var ^ " not yet implemented")
  else
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
let exec instr (frms : frame list) (stck : stack) (envs : env_stack) =
  match instr with
    IPushEnv cntxt -> (frms, stck, push_env (inst_context cntxt) envs)
  | IPopEnv ->
      (match envs with
         (_ :: envs) :: rest -> (frms, stck, envs :: rest)
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
        ((WhileFrame body_frame, body_frame) :: frms, stck, envs)
      else
        (frms, stck, envs)
  | IRepeat body_frame ->
      let (times, stck) = pop_int stck in
      if times > 0 then
        ((RepeatFrame (times - 1, body_frame), body_frame) :: frms, stck, envs)
      else
        (frms, stck, envs)
  | IBreak ->
      (match (frms, envs) with
         ((WhileFrame _, _) :: frms, _ :: envs) -> (frms, stck, envs)
       | (_ :: frms, _ :: envs) -> error "cannot break out of a non-loop frame"
       | _ -> error "missing a frame or environment to pop")
  | IPrint count -> (frms, (print count stck), envs)
  | ICast t ->
      let (v, stck) = pop stck in
      (match t, v with
         BoolType, IntData i -> (frms, push (BoolData (i != 0)) stck, envs)
       | _ -> error ("cannot convert " ^ (string_of_type (type_of_data v)) ^ " to " ^ (string_of_type t)))
  | IAdd | ISubtract | IMultiply | IDivide | ILessThan | IGreaterThan -> (frms, exec_binop instr stck, envs)
  | IPreInc v | IPostInc v | IPreDec v | IPostDec v ->
      let slot = (find_mem (first_env_list envs) v) in
      (match !slot with
         IntData i ->
           let newval, retval =
             (match instr with
                IPreInc _ -> i+1, i+1
              | IPostInc _ -> i+1, i
              | IPreDec _ -> i-1, i-1
              | IPostDec _ -> i-1, i
              | _ -> error "cannot happen")
           in slot := IntData newval; (frms, (IntData retval) :: stck, envs)
       | _ -> error ("incr/decr applied to invalid data type, " ^ (string_of_type (type_of_data !slot))))
  | IYield -> error "run_til_yield passed IYield to exec"

(* executes instructions in the given environments until it yields or finishes *)
(* returns the number of samples yielded and the new execution state *)
let rec run_til_yield (state : execution_state) =
  match state with
    ([], [], _) -> None
  | ((ft, IYield::is) :: frms, stck, envs) -> let d, stck = pop_dur stck in Some (d, ((ft, is) :: frms, (TimeData 0.0 (* HACK *)) :: stck, envs))
  | ((ft, i::is) :: frms, stck, envs) -> run_til_yield (exec i ((ft, is)::frms) stck envs)
  | ((Frame, []) :: frms, stck, envs) -> run_til_yield (frms, stck, envs)
  | ((WhileFrame body, []) :: frms, stck, envs) ->
      let (cond, stck) = pop_bool stck in
      if cond then
        run_til_yield ((WhileFrame body, body) :: frms, stck, envs)
      else
        run_til_yield (frms, stck, envs)
  | ((RepeatFrame (times, body), []) :: frms, stck, envs) ->
      if times > 0 then
        run_til_yield ((RepeatFrame (times - 1, body), body) :: frms, stck, envs)
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
    type vm = time * Shred.shred Priority_queue.queue (* now, shreds *)
    let fresh = (0.0, Priority_queue.empty)
    let add (now, q) (cntxt, funcs, instrs) =
      let shred = Shred.shred now ([(Frame, instrs)], [], [(inst_context cntxt) :: [(* TODO: global VM env *)]]) in
      (now, Priority_queue.insert q now shred)
    let re_add (now, q) shred = (now, Priority_queue.insert q (Shred.now shred) shred)
    let set_time (now, q) time = (time, q)
    let next_shred (now, q) = let _, shred, q' = Priority_queue.extract q in (shred, (now, q'))
    let running (now, q) = not (Priority_queue.is_empty q)
    let rec run max_samples ((now, q) as vm) =
      try
        let shred, vm = next_shred vm in
        if (Shred.now shred) -. now >= max_samples then (* no changes to DSP change in this time *)
          set_time vm (now +. max_samples)
        else
          match run_til_yield (Shred.state shred) with
            None -> run max_samples vm (* shred finished *)
          | Some(elapsed, state) ->
              let vm = re_add vm (Shred.shred ((Shred.now shred) +. elapsed) state) in
              set_time vm (now +. max_samples)
      with Priority_queue.Queue_is_empty ->
        set_time vm (now +. max_samples) (* no shreds *)
  end
