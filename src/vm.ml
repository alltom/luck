
open Compile

type frame = TopLevel | LoopFrame

let rec run instrs =
  let rec real_run stack =
    match stack with
      [] -> ()
    | (_, []) :: stack' -> real_run stack'
    | (t, (instr :: instrs)) :: stack' ->
        (match instr with
           Op f -> f (); real_run ((t, instrs) :: stack')
         | Branch (cond, iftrue, iffalse) ->
             if !(!cond) then
               real_run ((t, (iftrue @ instrs)) :: stack')
             else
               real_run ((t, (iffalse @ instrs)) :: stack')
         | Loop (cond, body) ->
             if !(!cond) then
               real_run ((t, (body @ instrs @ [Loop(cond, body)])) :: stack')
             else
               real_run ((t, instrs) :: stack')
         | Break -> real_run stack')
  in
  real_run [(TopLevel, instrs)]
