
open Compile

let rec run instrs =
  let rec real_run stack =
    match stack with
      [] -> ()
    | [] :: stack' -> real_run stack'
    | (instr :: instrs) :: stack' ->
        (match instr with
           Op f -> f (); real_run (instrs :: stack')
         | Branch (cond, iftrue, iffalse) ->
             if !(!cond) then
               real_run ((iftrue @ instrs) :: stack')
             else
               real_run ((iffalse @ instrs) :: stack'))
  in
  real_run [instrs]
