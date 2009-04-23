
exception Compile_error of string (* something is wrong with the input *)
exception Compiler_error of string (* something went wrong internally *)
exception Not_implemented of string
exception Type_mismatch of string

val compile : Vm.context -> Ast.func list * Ast.clas list * Ast.stmt list -> Vm.shred_template
