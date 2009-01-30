
open Compile

let run_instr stack instr =
  match instr with
    Op f -> f (); stack
  | Branch (cond, iftrue, iffalse) -> if !(!cond) then iftrue :: stack else iffalse :: stack

let rec run instrs =
  let rec real_run stack =
    match stack with
      [] -> ()
    | [] :: stack -> real_run stack
    | (hd :: tl) :: stack -> real_run (run_instr (tl :: stack) hd)
  in
  real_run [instrs]
