
open Ast

let main () =
  try
    let tree = Parser.input Lexer.token (Lexing.from_channel stdin) in
    ast_summary tree
  with Parsing.Parse_error -> exit 1

let _ = main ()
