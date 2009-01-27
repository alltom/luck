
open Compile

let instr_print args =
  let rec pair_of_data d =
    match d with
      ArrayData arr -> ("array", "array")
    | RefData d' -> let (v, t) = pair_of_data !d' in (v, t ^ " ref")
    | IntData i -> (string_of_int !i, "int")
    | BoolData b -> (string_of_bool !b, "bool")
    | FloatData f -> (string_of_float !f, "float")
    | StringData s -> (!s, "string")
  in
  let string_of_pair (v, t) = v ^ " :(" ^ t ^ ")" in
  print_endline (match args with
    d :: [] -> string_of_pair (pair_of_data d)
  | _ -> String.concat " " (List.map (fun d -> let (v, _) = pair_of_data d in v) args))

let run_instr instr =
  match instr with
    PrintInstr args -> instr_print args

let rec run instrs =
  match instrs with
    i :: rest -> ignore(run_instr i); run rest
  | [] -> ()
