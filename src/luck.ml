
open Compiler
open Interpreter
open Vm

let arg_print_instrs = ref false
let window_size = ref 100.0
let filenames = ref []
let usage = "usage: " ^ Sys.argv.(0) ^ " [-i] [-w window_size] files [...]"

let speclist = [
  ("-i", Arg.Unit (fun () -> arg_print_instrs := true), " print instructions before execution");
  ("-w", Arg.Int (fun w -> window_size := float_of_int w), " window size");
  ("--", Arg.Rest (fun fname -> filenames := !filenames @ [fname]), " files to load (- for stdin)");
]

let print_instrs name instrs =
  print_endline (name ^ ":");
  List.iter (fun i -> print_endline ("\t" ^ (string_of_instruction i))) instrs;
  print_endline "---"

let load_file name =
  let ch = if name = "-" then stdin else open_in name in
  Parser.input Lexer.token (Lexing.from_channel ch)

let main () =
  Arg.parse speclist (fun fname -> filenames := !filenames @ [fname]) usage;
  try
    if List.length !filenames = 0 then
      (prerr_endline "[luck]: no input files... (try --help)";
       exit 1);
    let vm = List.fold_left
      (fun vm n -> let shred = compile (VM.global_context vm) (load_file n) in
                   if !arg_print_instrs then print_instrs n (shred_instructions shred);
                   VM.add vm shred)
      VM.fresh !filenames
    in
    let rec run vm =
      let vm = VM.run !window_size vm in
      if VM.running vm then run vm else ()
    in
    run vm
  with
    Parsing.Parse_error -> prerr_endline ("parse error"); exit 1
  | Compile_error msg -> prerr_endline ("compile error: " ^ msg); exit 1
  | Compiler_error msg -> prerr_endline ("compiler error: " ^ msg); exit 1
  | Compiler.Not_implemented msg
  | Vm.Not_implemented msg -> prerr_endline ("not implemented: " ^ msg); exit 1

let _ = main ()
