
open Ast
open Compile
open Vm

let arg_print_instrs = ref false
let usage = "usage: " ^ Sys.argv.(0) ^ " [-i]"

let speclist = [
  ("-i", Arg.Unit (fun () -> arg_print_instrs := true), ": print instructions before execution");
]

let main () =
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
  let tree =
    try Parser.input Lexer.token (Lexing.from_channel stdin)
    with Parsing.Parse_error -> exit 1
  in
  let (cntxt, funcs, instrs) =
    try compile Context.empty tree
    with
      Compile_error msg -> prerr_endline ("compile error: " ^ msg); exit 1
    | Compiler_error msg -> prerr_endline ("compiler error: " ^ msg); exit 1
    | Not_implemented msg -> prerr_endline ("not implemented: " ^ msg); exit 1
  in
  if !arg_print_instrs then
    (List.iter (fun i -> print_endline ("\t" ^ (string_of_instruction i))) instrs;
    print_endline "---")
  else ();
  run_til_yield (instantiate_shred Env.empty (cntxt, funcs, instrs))

let _ = main ()
