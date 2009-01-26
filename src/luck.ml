
open Ast

let main () =
  try
    let tree = Parser.input Lexer.token (Lexing.from_channel stdin) in
    ignore(tree);
    print_endline "done!"
  with Parsing.Parse_error -> exit 1

let _ = main ()
