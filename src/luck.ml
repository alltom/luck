
open Ast
open Compile
open Vm

let main () =
  let tree =
    try Parser.input Lexer.token (Lexing.from_channel stdin)
    with Parsing.Parse_error -> exit 1
  in
  ast_summary tree;
  let instrs =
    try compile tree
    with
      Compiler_error msg -> prerr_endline ("compiler error: " ^ msg); exit 1
    | Not_implemented msg -> prerr_endline ("not implemented: " ^ msg); exit 1
    | Redeclaration -> prerr_endline "variable redeclaration"; exit 1
    | Undeclared_variable v -> prerr_endline ("using variable " ^ v ^ " without declaring it"); exit 1
  in
  run instrs

let _ = main ()
