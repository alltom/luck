
open Ast
open Compile

let num_tests = ref 0
let num_failed = ref 0

let strings_of_test_cntxt cntxt =
  let f (name, t) = (string_of_type t) ^ " " ^ name in
  List.map (f) cntxt

let strings_of_real_cntxt cntxt =
  Context.fold (fun name t lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

let rec check_vars cntxt arr =
  match arr with
    (name, t) :: rest ->
      (Context.mem name cntxt)
        && ((Context.find name cntxt) = t)
        && (check_vars cntxt rest)
  | [] -> true

let fail msg =
  num_failed := !num_failed + 1;
  prerr_endline ("FAILED: " ^ msg)

(* assertion *)
let t stmts cntxt code =
  num_tests := !num_tests + 1;
  try
    let rescntxt, rescode = compile Context.empty stmts in
    if check_vars rescntxt cntxt then
      (if rescode = code then
         print_endline ("passed")
       else fail "code didn't match")
    else
      fail ("contexts didn't match: "
            ^ "expected " ^ (String.concat ", " (strings_of_test_cntxt cntxt)) ^ ", "
            ^ "got " ^ (String.concat ", " (strings_of_real_cntxt rescntxt)))
  with _ -> fail "compiler exception"

let _ =
  let es e = ExprStatement(e) in
  
  (* constant expressions with no side effects
     should compile to nothing *)
  t NullStatement [] [];
  t (es (Int 1)) [] [];
  t (es (Plus(Int 1, Int 2))) [] [];
  
  (* declarations should become part of the local context *)
  t (es (Declaration [("a", Type("int", false, false, []))])) [("a", IntType)] [];
  t (es (Declaration [("b", Type("int", false, false, [Dynamic]))])) [("b", ArrayType(IntType))] [];
  t (es (Declaration [("b", Type("int", false, false, [Dynamic; Dynamic]))])) [("b", ArrayType(ArrayType(IntType)))] [];
  
  print_endline ("failed " ^ (string_of_int !num_failed) ^ " of " ^ (string_of_int !num_tests))
