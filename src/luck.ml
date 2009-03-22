
open Ast
open Compile
open Vm

let main () =
  let tree =
    try Parser.input Lexer.token (Lexing.from_channel stdin)
    with Parsing.Parse_error -> exit 1
  in
  let (cntxt, instrs) =
    try compile tree
    with
      Compiler_error msg -> prerr_endline ("compiler error: " ^ msg); exit 1
    | Not_implemented msg -> prerr_endline ("not implemented: " ^ msg); exit 1
    | Redeclaration -> prerr_endline "variable redeclaration"; exit 1
    | Undeclared_variable v -> prerr_endline ("using variable " ^ v ^ " without declaring it"); exit 1
  in
  List.iter (fun i -> print_endline ("\t" ^ (string_of_instruction i))) instrs;
  print_endline "---";
  run instrs (inst_context cntxt)

let _ = main ()
