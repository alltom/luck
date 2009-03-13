
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
| IPrint of int (* number of things to print (consumes) *)

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

let string_of_instruction = function
  IPush d -> "push value " ^ (string_of_data d)
| IDiscard -> "discard"
| IInit (v, d) -> "init " ^ v ^ " = " ^ (string_of_data d)
| IPushVar v -> "push var " ^ v
| IAssign s -> "assign " ^ s
| IPrint i -> "print " ^ (string_of_int i)
  
let error msg =
  raise (Machine_error msg)

let pop = function
  d :: stck -> (d, stck)
| _ -> error "stack underflow"

let rec nfold f memo n =
  if n > 0 then nfold f (f memo) (n-1) else memo

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
  | IPrint count -> (frms, (print count stck), envs)

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
