
open Compile
open Vm

let arg_print_instrs = ref false
let window_size = ref 100.0
let usage = "usage: " ^ Sys.argv.(0) ^ " [-i] [-w window_size]"

let speclist = [
  ("-i", Arg.Unit (fun () -> arg_print_instrs := true), " print instructions before execution");
  ("-w <num>", Arg.Int (fun w -> window_size := float_of_int w), " window size");
]

let print_instrs instrs =
  List.iter
    (fun i -> print_endline ("\t" ^ (string_of_instruction i)))
    instrs;
  print_endline "---"

let main () =
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
  try
    let tree = Parser.input Lexer.token (Lexing.from_channel stdin) in
    let shred = Compile.compile Vm.Context.empty tree in
    if !arg_print_instrs then print_instrs (shred_instructions shred);
    let vm = VM.add VM.empty shred in
    let rec run vm =
      let vm = VM.run !window_size vm in
      if VM.running vm then run vm else ()
    in
    run vm
  with
    Parsing.Parse_error -> prerr_endline ("parse error"); exit 1
  | Compile_error msg -> prerr_endline ("compile error: " ^ msg); exit 1
  | Compiler_error msg -> prerr_endline ("compiler error: " ^ msg); exit 1
  | Not_implemented msg -> prerr_endline ("not implemented: " ^ msg); exit 1

let _ = main ()
