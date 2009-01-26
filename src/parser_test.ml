
open Parser
open Ast

let make_tree str =
  Parser.input Lexer.token (Lexing.from_string str)

let assert_tree str tree =
  (if (make_tree str) = tree then
     print_endline ("passed: " ^ str)
   else
     prerr_endline ("failed: " ^ str))

let _ =
  assert_tree "" (AST([], [], []));
  print_endline "done!"
