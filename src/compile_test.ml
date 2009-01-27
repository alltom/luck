
open Ast
open Compile

let num_tests = ref 0
let num_failed = ref 0

let strings_of_test_cntxt cntxt =
  let f (name, t) = (string_of_type t) ^ " " ^ name in
  List.map (f) cntxt

let strings_of_real_cntxt cntxt =
  Context.fold (fun name (t, d) lst -> ((string_of_type t) ^ " " ^ name) :: lst) cntxt []

let rec check_vars cntxt arr =
  match arr with
    (name, t) :: rest ->
      (Context.mem name cntxt)
        && (let (t', _) = (Context.find name cntxt) in t' = t)
        && (check_vars cntxt rest)
  | [] -> true

let fail msg =
  num_failed := !num_failed + 1;
  prerr_endline ("FAILED: " ^ msg)

(* assertion *)
let t stmt cntxt code =
  num_tests := !num_tests + 1;
  try
    let rescntxt, rescode = compile_stmt Context.empty stmt in
    if check_vars rescntxt cntxt then
      (if rescode = code then
         print_endline ("passed")
       else
         fail "code didn't match")
    else
      fail ("contexts didn't match: "
            ^ "expected [" ^ (String.concat ", " (strings_of_test_cntxt cntxt)) ^ "], "
            ^ "got [" ^ (String.concat ", " (strings_of_real_cntxt rescntxt)) ^ "]")
  with
    Type_declaration -> fail "compiler error: type declaration"
  | _ -> fail "compiler exception"

let _ =
  let int_a = Declaration [("a", Type("int", false, false, []))] in
  let int_b = Declaration [("b", Type("int", false, false, []))] in
  let int_c = Declaration [("c", Type("int", false, false, []))] in
  let es e = ExprStatement(e) in
  
  (* constant expressions with no side effects *)
  print_endline "  constant expressions";
  t NullStatement [] [];
  t (es (Int 1)) [] [];
  t (es (BinaryExpr(Plus, Int 1, Int 2))) [] [];
  
  (* simple declarations should become part of the local context *)
  print_endline "  simple declarations";
  t (es (int_a)) [("a", IntType)] [];
  t (es (Declaration [("b", Type("int", false, false, [Dynamic]))])) [("b", ArrayType(IntType))] [];
  t (es (Declaration [("b", Type("int", false, false, [Dynamic; Dynamic]))])) [("b", ArrayType(ArrayType(IntType)))] [];
  t (es (Declaration [("a", Type("float", false, false, []))])) [("a", FloatType)] [];
  t (es (Declaration [("a", Type("string", false, false, []))])) [("a", StringType)] [];
  t (es (Declaration [("a", Type("int", true, false, []))])) [("a", RefType(IntType))] [];
  t (es (Declaration [("a", Type("int", true, false, [Dynamic]))])) [("a", RefType(ArrayType(IntType)))] [];
  t (es (Declaration [("a", Type("int", false, false, [Fixed (Int 1)]))])) [("a", ArrayType(IntType))] [];

  (* nested declarations should become part of the local context *)
  print_endline "  nested declarations";
  t (es (Array [int_a])) [("a", IntType)] [];
  t (es (Array [int_a; int_b])) [("a", IntType); ("b", IntType)] [];
  t (es (UnaryExpr(ArithNegation, int_a))) [("a", IntType)] [];
  t (es (BinaryExpr(Chuck, Int 1, int_a))) [("a", IntType)] [];
  t (es (Member(int_a, "b"))) [("a", IntType)] [];
  t (es (FunCall(int_a, []))) [("a", IntType)] [];
  t (es (FunCall(Var "a", [int_a]))) [("a", IntType)] [];
  t (es (FunCall(int_a, [int_b]))) [("a", IntType); ("b", IntType)] [];
  t (es (FunCall(int_a, [int_b; int_c]))) [("a", IntType); ("b", IntType); ("c", IntType)] [];
  t (es (Cast(int_a, Type("int", false, false, [])))) [("a", IntType)] [];
  t (es (Spork int_a)) [("a", IntType)] [];
  t (es (Trinary(int_a, Int 1, Int 2))) [("a", IntType)] [];
  t (es (Trinary(Int 1, int_a, Int 2))) [("a", IntType)] [];
  t (es (Trinary(Int 1, Int 2, int_a))) [("a", IntType)] [];
  t (es (Trinary(int_a, int_b, int_c))) [("a", IntType); ("b", IntType); ("c", IntType)] [];
  
  (* simple commands *)
  print_endline "  simple commands";
  t (Print [Int 1]) [] [PrintInstr [IntData (ref 1)]];
  t (Print [Int 1; Bool false]) [] [PrintInstr [IntData (ref 1); BoolData (ref false)]];
  
  print_endline ("failed " ^ (string_of_int !num_failed) ^ " of " ^ (string_of_int !num_tests))
