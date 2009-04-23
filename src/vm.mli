
exception Machine_error of string

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

module Context :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end

type context = typ Context.t

val builtin_variables : (string * typ) list
val builtin_type : string -> typ
val builtin_cntxt : typ Context.t
val is_builtin : string -> bool

type instruction =
  IPushEnv of context
| IPopEnv
| IPush of data
| IDiscard
| IPushVar of string
| IAssign of string
| IBranch of instruction list * instruction list
| IWhile of instruction list
| IBreak
| IPrint of int
| ICast of typ * typ
| IAdd
| ISubtract
| IMultiply
| IDivide
| ILessThan
| IGreaterThan
| IYield

type func = typ * typ list * instruction list
type shred_template = context * func list * instruction list

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
    type vm = time * Shred.shred Priority_queue.queue
    val empty : vm
    val add : vm -> shred_template -> vm
    val run : time -> vm -> vm
  end
