
exception Machine_error of string
exception Not_implemented of string

type time = float

type typ =
  ArrayType of array_dimension * typ
| RefType of typ
| IntType
| FloatType
| BoolType
| StringType
| DurType
| TimeType
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

module Context : Map.S with type key = String.t
type context = typ Context.t

val builtin_variables : (string * typ) list
val builtin_type : string -> typ
val builtin_cntxt : typ Context.t
val is_builtin : string -> bool

type instruction =
  IPushEnv of context (* instantiates the context and pushes env onto stack *)
| IPopEnv (* pops the top-most env *)
| IPush of data (* pushes a value onto the stack *)
| IDiscard (* discards the top stack value *)
| IPushVar of string (* pushes current value of var onto stack *)
| IAssign of string (* puts top stack value in variable with given name; leaves value on stack *)
| IBranch of instruction list * instruction list (* pops stack value; if true, execs first list, else the other *)
| IWhile of instruction list (* repeats instructions until top stack value is false *)
| IRepeat of instruction list (* pops int from stack, repeats instructions that number of times *)
| IBreak (* pops a loop frame and an environment *)
| IPrint of int (* pops and prints the given number of stack values to stdout *)
| ICast of typ (* converts value at top of stack to this type *)
| IAdd | ISubtract | IMultiply | IDivide (* replaces top two stack values with the result *)
| ILessThan | IGreaterThan (* replaces top two stack values with the comparison *)
| IPreInc of string | IPostInc of string (* ++a and a++ *)
| IPreDec of string | IPostDec of string (* --a and a-- *)
| IYield (* pops a dur and yields that number of samples *)

type func = typ * typ list * instruction list
type shred_template = context * func list * instruction list

val shred_instructions : shred_template -> instruction list

module Env :
  sig
    type environment
    val empty : environment
    val add_func : string -> func -> environment -> environment
    val add_mem : string -> data ref -> environment -> environment
    val find_func : string -> environment -> func
    val find_mem : string -> environment -> data ref
  end

type execution_state

module Shred :
  sig
    type shred
    val shred : time -> execution_state -> shred
    val now : shred -> time
    val state : shred -> execution_state
  end

val string_of_type : typ -> string
val strings_of_cntxt : (typ * 'a) Context.t -> string list
val string_of_data : data -> string
val string_of_instruction : instruction -> string

module VM :
  sig
    type vm
    val fresh : vm
    val add : vm -> shred_template -> vm
    val run : time -> vm -> vm (* max samples *)
    val running : vm -> bool
  end
