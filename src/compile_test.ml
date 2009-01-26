
open Ast
open Compile

let num_tests = ref 0
let num_failed = ref 0

let rec check_types cntxt arr =
  match arr with
    (name, t) :: rest ->
      (Context.mem name cntxt)
        && ((Context.find name cntxt) = t)
        && (check_types cntxt rest)
  | [] -> true

let fail () =
  num_failed := !num_failed + 1;
  prerr_endline ("FAILED")

(* assert_tree *)
let t stmts cntxt code =
  num_tests := !num_tests + 1;
  try
    let rescntxt, rescode = compile Context.empty stmts in
    if (check_types rescntxt cntxt) && (rescode = code) then
      print_endline ("passed")
    else
      fail ()
  with _ -> fail ()

let _ =
  let es e = ExprStatement(e) in
  
  (* constant expressions with no side effects
     should compile to nothing *)
  t NullStatement [] [];
  t (es (Int 1)) [] [];
  t (es (Plus(Int 1, Int 2))) [] [];
  
  (* declarations should become part of the local context *)
  t (es (Declaration [("a", Type("int", false, false, []))])) [("a", IntType)] [];
  
  print_endline ("failed " ^ (string_of_int !num_failed) ^ " of " ^ (string_of_int !num_tests))
