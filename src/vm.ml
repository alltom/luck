
open Compile

let run_instr instr =
  match instr with
    Op f -> f ()

let rec run instrs =
  match instrs with
    i :: rest -> ignore(run_instr i); run rest
  | [] -> ()
