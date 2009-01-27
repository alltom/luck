
open Compile

let run_instr instr =
  match instr with
    _ -> raise (Not_implemented "don't know how to execute instructions yet")

let rec run instrs =
  match instrs with
    i :: rest -> ignore(run_instr i); run rest
  | [] -> ()
