
exception Compile_error of string (* something is wrong with the input *)
exception Compiler_error of string (* something went wrong internally *)
exception Not_implemented of string

val compile : Interpret.context -> Ast.ast -> Vm.shred_template
