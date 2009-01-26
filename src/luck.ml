
open Ast

let print_summary filename (AST(fns, classes, stmts)) =
  let ip pref str = print_endline (pref ^ str) in
  let function_summary pref (Function(typ, name, decl, stmts)) =
    let p = ip pref in
    p ("function: " ^ name);
    p ("  type: " ^ (string_of_type typ))
  in
  let class_summary pref (Class(public, name, parents, fns, stmts)) =
    let p = ip pref in
    p ((if public then "public " else "") ^ "class: " ^ name);
    p ("  parents: " ^ (String.concat ", " parents));
    p "  methods:";
    List.iter (function_summary (pref ^ "    ")) fns
  in
  print_endline ("summary: " ^ filename);
  List.iter (class_summary "") classes;
  print_endline "functions:";
  List.iter (function_summary "  ") fns

let main () =
  try
    let tree = Parser.input Lexer.token (Lexing.from_channel stdin) in
    print_summary "<stdin>" tree
  with Parsing.Parse_error -> exit 1

let _ = main ()
