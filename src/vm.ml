
open Interpret

exception Machine_error of string
exception Not_implemented of string
let error msg = raise (Machine_error msg)

(* let builtin_cntxt =
  List.fold_left
    (fun cntxt (name, t) -> Context.add name t cntxt)
    Context.empty
    builtin_variables *)

type shred_template = context * func list * instruction list

(* returns the instructions from a shred template *)
let shred_instructions (_, _, instrs) = instrs

module Shred =
  struct
    type shred = time * execution_state
    let shred now state = (now, state)
    let now shred = let (now, _) = shred in now
    let state shred = let (_, state) = shred in state
  end

(* maintains a sense of "now" given a set of shreds *)
(* TODO: instead of putting a shred back in the queue, run it
         repeatedly until it is no longer the furthest behind *)
module VM =
  struct
    type vm = time * Shred.shred Priority_queue.queue (* now, shreds *)
    let fresh = (0.0, Priority_queue.empty)
    let add (now, q) (cntxt, funcs, instrs) =
      let shred = Shred.shred now ([(Frame, instrs)], [], [(inst_context cntxt) :: [(* TODO: global VM env *)]]) in
      (now, Priority_queue.insert q now shred)
    let re_add (now, q) shred = (now, Priority_queue.insert q (Shred.now shred) shred)
    let set_time (now, q) time = (time, q)
    let next_shred (now, q) = let _, shred, q' = Priority_queue.extract q in (shred, (now, q'))
    let running (now, q) = not (Priority_queue.is_empty q)
    let rec run max_samples ((now, q) as vm) =
      try
        let shred, vm = next_shred vm in
        if (Shred.now shred) -. now >= max_samples then (* no changes to DSP change in this time *)
          set_time vm (now +. max_samples)
        else
          match run_til_yield (Shred.state shred) with
            None -> run max_samples vm (* shred finished *)
          | Some(elapsed, state) ->
              let vm = re_add vm (Shred.shred ((Shred.now shred) +. elapsed) state) in
              set_time vm (now +. max_samples)
      with Priority_queue.Queue_is_empty ->
        set_time vm (now +. max_samples) (* no shreds *)
  end
