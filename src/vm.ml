
type instruction =
  Op of (unit -> unit)
| Branch of bool ref ref * instruction list * instruction list
| Loop of bool ref ref * instruction list
| Break

type frame = TopLevel | LoopFrame of bool ref ref * instruction list

let rec run instrs =
  let rec real_run stack =
    match stack with
      [] -> ()
    | (t, []) :: stack' ->
        (match t with
           TopLevel -> real_run stack'
         | LoopFrame(cond, body) ->
             if !(!cond) then
               real_run ((t, body) :: stack')
             else
               real_run stack')
    | (t, (instr :: instrs)) :: stack' ->
        (match instr with
           Op f -> f (); real_run ((t, instrs) :: stack')
         | Branch(cond, iftrue, iffalse) ->
             if !(!cond) then
               real_run ((t, (iftrue @ instrs)) :: stack')
             else
               real_run ((t, (iffalse @ instrs)) :: stack')
         | Loop(cond, body) ->
             if !(!cond) then
               real_run ((LoopFrame(cond, body), body) :: (t, instrs) :: stack')
             else
               real_run ((t, instrs) :: stack')
         | Break -> real_run stack')
  in
  real_run [(TopLevel, instrs)]
