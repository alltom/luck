
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
let t stmts cntxt code =
  num_tests := !num_tests + 1;
  try
    let rescntxt, rescode = compile Context.empty stmts in
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
  let es e = ExprStatement(e) in
  
  (* constant expressions with no side effects *)
  print_endline "  constant expressions";
  t NullStatement [] [];
  t (es (Int 1)) [] [];
  t (es (Plus(Int 1, Int 2))) [] [];
  
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
  t (es (ArithNegation int_a)) [("a", IntType)] [];
  t (es (Negation int_a)) [("a", IntType)] [];
  t (es (PreInc int_a)) [("a", IntType)] [];
  t (es (PostInc int_a)) [("a", IntType)] [];
  t (es (PreDec int_a)) [("a", IntType)] [];
  t (es (PostDec int_a)) [("a", IntType)] [];
  t (es (Member(int_a, "b"))) [("a", IntType)] [];
  t (es (Chuck(Int 1, int_a))) [("a", IntType)] [];
  t (es (Chuck(int_a, Int 1))) [("a", IntType)] [];
  t (es (Plus(Int 1, int_a))) [("a", IntType)] [];
  t (es (Plus(int_a, Int 1))) [("a", IntType)] [];
  
  print_endline ("failed " ^ (string_of_int !num_failed) ^ " of " ^ (string_of_int !num_tests))

(*
  | Subscript of expr * expr
  | Chuck of expr * expr
  | Unchuck of expr * expr
  | Upchuck of expr * expr
  | Atchuck of expr * expr
  | Minuschuck of expr * expr
  | Pluschuck of expr * expr
  | Cast of expr * typ
  | Time of expr * expr
  | Spork of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr
  | Modulo of expr * expr
  | Exponentiate of expr * expr
  | LessThan of expr * expr
  | LessThanOrEqualTo of expr * expr
  | GreaterThan of expr * expr
  | GreaterThanOrEqualTo of expr * expr
  | Equals of expr * expr
  | NotEquals of expr * expr
  | BinaryAnd of expr * expr
  | BinaryOr of expr * expr
  | Trinary of expr * expr * expr
  | Declaration of decl
  | Comma of expr * expr
*)
