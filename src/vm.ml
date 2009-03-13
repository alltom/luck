
type data =
  ArrayData of data array
| RefData of data
| IntData of int
| BoolData of bool
| FloatData of float
| StringData of string

and instruction =
  IPush of data
| IDiscard
| IInit of string * data
| IPushVar of string
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of frame * frame (* if true body, if false body *)
| IWhile of frame * frame (* condition, body *)
| IPrint of int (* number of things to print (consumes) *)
| IBoolCast
| IAddInt

and frame = instruction list

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
  IPush d -> "push value " ^ (string_of_data d)
| IDiscard -> "discard"
| IInit (v, d) -> "init " ^ v ^ " = " ^ (string_of_data d)
| IPushVar v -> "push var " ^ v
| IAssign s -> "assign " ^ s
| IBranch (f1, f2) -> "if (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") (" ^ (String.concat "; " (List.map string_of_instruction f2)) ^ ")"
| IWhile (f1, f2) -> "while (" ^ (String.concat "; " (List.map string_of_instruction f1)) ^ ") { " ^ (String.concat "; " (List.map string_of_instruction f2)) ^ " }"
| IPrint i -> "print " ^ (string_of_int i)
| IBoolCast -> "cast to bool"
| IAddInt -> "add (int)"
  
let error msg =
  raise (Machine_error msg)

let rec nfold f memo n =
  if n > 0 then nfold f (f memo) (n-1) else memo

let pop = function
  d :: stck -> (d, stck)
| _ -> error "stack underflow"

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

let exec instr frms stck envs =
  match instr with
    IPush d -> (frms, d :: stck, envs)
  | IDiscard -> let (v, stck) = pop stck in (frms, stck, envs)
  | IInit (v, d) -> (match envs with env::rest -> (frms, stck, (env_insert v d env) :: rest) | [] -> error "weird environment")
  | IPushVar var -> (frms, !(lookup envs var) :: stck, envs)
  | IAssign var -> let (v, stck) = pop stck in assign envs var v; (frms, v :: stck, envs)
  | IBranch (f1, f2) ->
      let (cond, stck) = pop_bool stck in
      ((if cond then f1 else f2) :: frms, stck, envs)
  | IWhile (condframe, bodyframe) ->
      let (cond, stck) = pop_bool stck in
      if cond then
        (match frms with
           f1 :: rest -> ((bodyframe @ condframe) :: (instr :: f1) :: rest, stck, envs)
         | _ -> error "weird stack: expecting a frame")
      else
        (frms, stck, envs)
  | IPrint count -> (frms, (print count stck), envs)
  | IBoolCast ->
      let (v, stck) = pop stck in
      (match v with
         IntData i -> (frms, (BoolData (i != 0)) :: stck, envs)
       | BoolData b -> (frms, v :: stck, envs)
       | _ -> error "cannot convert to bool")
  | IAddInt ->
      let (i1, stck) = pop_int stck in
      let (i2, stck) = pop_int stck in
      (frms, (IntData (i1 + i2)) :: stck, envs)

let run frm env =
  let rec loop (frms, stck, envs) =
    match (frms, stck, envs) with
      ([], [v], _) -> Some v
    | ([], [], _) -> None
    | ((i::is) :: frms, stck, envs) -> loop (exec i (is::frms) stck envs)
    | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
    | _ -> error "invalid machine state"
  in
  loop ([frm], [], [env])
