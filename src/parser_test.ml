
open Parser
open Ast

let make_tree str =
  Parser.input Lexer.token (Lexing.from_string str)

let num_tests = ref 0
let num_failed = ref 0

(* assert_tree *)
let t str fns classes stmts =
  num_tests := !num_tests + 1;
  (if (try (make_tree str) = AST(fns, classes, stmts) with Parsing.Parse_error -> false) then
     print_endline ("passed: " ^ str)
   else
     (num_failed := !num_failed + 1;
      prerr_endline ("FAILED: " ^ str)))

let _ =
  let a = Var("a") in
  let b = Var("b") in
  let c = Var("c") in
  let d = Var("d") in
  let e = Var("e") in
  let es e = ExprStatement(e) in

  (* c'mon, you gotta get at least this one right *)
  t "" [] [] [];
  
  (* fair-weather statements *)
  t ";" [] [] [NullStatement];
  t "a;" [] [] [es a];
  t "return;" [] [] [Return];
  t "return a;" [] [] [ValuedReturn(a)];
  t "<<< a >>>;" [] [] [Print([a])];
  t "<<< a, b >>>;" [] [] [Print([a; b])];
  t "while(a) b;" [] [] [While(a, [es b])];
  t "while(a){b;}" [] [] [While(a, [es b])];
  t "while(a){b;c;}" [] [] [While(a, [es b; es c])];
  t "do{a;}while(b);" [] [] [Do([es a], b)];
  t "do{a;b;}while(b);" [] [] [Do([es a; es b], b)];
  t "until(a) b;" [] [] [Until(a, [es b])];
  t "until(a){b;}" [] [] [Until(a, [es b])];
  t "until(a){b;c;}" [] [] [Until(a, [es b; es c])];
  t "if(a) b;" [] [] [If(a, [es b], [])];
  t "if(a){b;}" [] [] [If(a, [es b], [])];
  t "if(a){b;c;}" [] [] [If(a, [es b; es c], [])];
  t "if(a) b; else c;" [] [] [If(a, [es b], [es c])];
  t "if(a){b;}else{c;}" [] [] [If(a, [es b], [es c])];
  t "if(a){b;c;}else{d;}" [] [] [If(a, [es b; es c], [es d])];
  t "if(a) b; else{c;}" [] [] [If(a, [es b], [es c])];
  t "if(a) b; else{c;d;}" [] [] [If(a, [es b], [es c; es d])];
  t "if(a){b;}else c;" [] [] [If(a, [es b], [es c])];
  t "if(a){b;c;}else d;" [] [] [If(a, [es b; es c], [es d])];
  t "for(;;);" [] [] [For(NullExpression, NullExpression, NullExpression, [NullStatement])];
  t "for(a;;);" [] [] [For(a, NullExpression, NullExpression, [NullStatement])];
  t "for(;a;);" [] [] [For(NullExpression, a, NullExpression, [NullStatement])];
  t "for(;;a);" [] [] [For(NullExpression, NullExpression, a, [NullStatement])];
  t "for(;;) a;" [] [] [For(NullExpression, NullExpression, NullExpression, [es a])];
  t "for(a;b;c) d;" [] [] [For(a, b, c, [es d])];
  t "for(a;b;c){d;}" [] [] [For(a, b, c, [es d])];
  t "for(a;b;c){d;e;}" [] [] [For(a, b, c, [es d; es e])];
  
  (* fair-weather expressions *)
  t "4;" [] [] [es (Int 4)];
  t "4.;" [] [] [es (Float 4.)];
  t "true;" [] [] [es (Bool true)];
  t "false;" [] [] [es (Bool false)];
  t "a;" [] [] [es a];
  t "\"howdy\";" [] [] [es (String "howdy")];
  t "(a);" [] [] [es a];
  t "[a];" [] [] [es (Array [a])];
  t "[a, b];" [] [] [es (Array [a; b])];
  t "[a, b, c];" [] [] [es (Array [a; b; c])];
  t "a.b;" [] [] [es (Member(a, "b"))];
  t "-a;" [] [] [es (UnaryExpr(ArithNegation, a))];
  t "!a;" [] [] [es (UnaryExpr(Negation, a))];
  t "a++;" [] [] [es (UnaryExpr(PostInc, a))];
  t "++a;" [] [] [es (UnaryExpr(PreInc, a))];
  t "a--;" [] [] [es (UnaryExpr(PostDec, a))];
  t "--a;" [] [] [es (UnaryExpr(PreDec, a))];
  t "a();" [] [] [es (FunCall(a, []))];
  t "a(b);" [] [] [es (FunCall(a, [b]))];
  t "a(b, c);" [] [] [es (FunCall(a, [b; c]))];
  t "a(b, c, d);" [] [] [es (FunCall(a, [b; c; d]))];
  t "a[b];" [] [] [es (BinaryExpr(Subscript, a, b))];
  t "a[b][c];" [] [] [es (BinaryExpr(Subscript, BinaryExpr(Subscript, a, b), c))];
  t "a => b;" [] [] [es (BinaryExpr(Chuck, a, b))];
  t "a =< b;" [] [] [es (BinaryExpr(Unchuck, a, b))];
  t "a =^ b;" [] [] [es (BinaryExpr(Upchuck, a, b))];
  t "a @=> b;" [] [] [es (BinaryExpr(Atchuck, a, b))];
  t "a -=> b;" [] [] [es (BinaryExpr(Minuschuck, a, b))];
  t "a +=> b;" [] [] [es (BinaryExpr(Pluschuck, a, b))];
  t "a $ b;" [] [] [es (Cast(a, Type("b", false, false, [])))];
  t "a :: b;" [] [] [es (BinaryExpr(Time, a, b))];
  t "spork ~ a();" [] [] [es (Spork(FunCall(a, [])))];
  t "a + b;" [] [] [es (BinaryExpr(Plus, a, b))];
  t "a - b;" [] [] [es (BinaryExpr(Minus, a, b))];
  t "a * b;" [] [] [es (BinaryExpr(Multiply, a, b))];
  t "a / b;" [] [] [es (BinaryExpr(Divide, a, b))];
  t "a % b;" [] [] [es (BinaryExpr(Modulo, a, b))];
  t "a ^ b;" [] [] [es (BinaryExpr(Exponentiate, a, b))];
  t "a < b;" [] [] [es (BinaryExpr(LessThan, a, b))];
  t "a <= b;" [] [] [es (BinaryExpr(LessThanOrEqualTo, a, b))];
  t "a > b;" [] [] [es (BinaryExpr(GreaterThan, a, b))];
  t "a >= b;" [] [] [es (BinaryExpr(GreaterThanOrEqualTo, a, b))];
  t "a == b;" [] [] [es (BinaryExpr(Equals, a, b))];
  t "a != b;" [] [] [es (BinaryExpr(NotEquals, a, b))];
  t "a && b;" [] [] [es (BinaryExpr(BinaryAnd, a, b))];
  t "a || b;" [] [] [es (BinaryExpr(BinaryOr, a, b))];
  t "a ? b : c;" [] [] [es (Trinary(a, b, c))];
  t "int a;" [] [] [es (Declaration([("a", Type("int", false, false, []))]))];
  t "int @ a;" [] [] [es (Declaration([("a", Type("int", true, false, []))]))];
  t "int a[];" [] [] [es (Declaration([("a", Type("int", false, false, [Dynamic]))]))];
  t "int a[b];" [] [] [es (Declaration([("a", Type("int", false, false, [Fixed b]))]))];
  t "int a[][];" [] [] [es (Declaration([("a", Type("int", false, false, [Dynamic; Dynamic]))]))];
  t "int a[b][c];" [] [] [es (Declaration([("a", Type("int", false, false, [Fixed b; Fixed c]))]))];
  t "int @ a[][];" [] [] [es (Declaration([("a", Type("int", true, false, [Dynamic; Dynamic]))]))];
  t "int @ a[][], b;" [] [] [es (Declaration([("a", Type("int", true, false, [Dynamic; Dynamic])); ("b", Type("int", true, false, []))]))];
  t "int @ a[][], b[][][];" [] [] [es (Declaration([("a", Type("int", true, false, [Dynamic; Dynamic])); ("b", Type("int", true, false, [Dynamic; Dynamic; Dynamic]))]))];
  t "a, b;" [] [] [es (Comma [a; b])];
  t "a, b, c;" [] [] [es (Comma [a; b; c])];
  
  (* fair-weather functions *)
  t "fun a b(){}" [Function(Type("a", false, false, []), "b", [], [])] [] [];
  t "fun a[] b(){}" [Function(Type("a", false, false, [Dynamic]), "b", [], [])] [] [];
  t "fun a @ [] b(){}" [Function(Type("a", true, false, [Dynamic]), "b", [], [])] [] [];
  t "fun a @ [][] b(){}" [Function(Type("a", true, false, [Dynamic; Dynamic]), "b", [], [])] [] [];
  t "fun static a b(){}" [Function(Type("a", false, true, []), "b", [], [])] [] [];
  t "fun static a @ [][] b(){}" [Function(Type("a", true, true, [Dynamic; Dynamic]), "b", [], [])] [] [];
  t "fun a b(c d){}" [Function(Type("a", false, false, []), "b", [("d", Type("c", false, false, []))], [])] [] [];
  t "fun a b(c d, e f){}" [Function(Type("a", false, false, []), "b", [("d", Type("c", false, false, [])); ("f", Type("e", false, false, []))], [])] [] [];
  t "fun a b(c @ d){}" [Function(Type("a", false, false, []), "b", [("d", Type("c", true, false, []))], [])] [] [];
  t "fun a b(c d, e @ f){}" [Function(Type("a", false, false, []), "b", [("d", Type("c", false, false, [])); ("f", Type("e", true, false, []))], [])] [] [];
  t "fun a b(c d[], e @ f){}" [Function(Type("a", false, false, []), "b", [("d", Type("c", false, false, [Dynamic])); ("f", Type("e", true, false, []))], [])] [] [];
  t "fun a b(){a;}" [Function(Type("a", false, false, []), "b", [], [es a])] [] [];
  t "fun a b(){a;b;}" [Function(Type("a", false, false, []), "b", [], [es a; es b])] [] [];
  
  (* fair-weather classes *)
  t "class a { }" [] [Class(false, "a", [], [], [])] [];
  t "public class a { }" [] [Class(true, "a", [], [], [])] [];
  t "class a extends b { }" [] [Class(false, "a", ["b"], [], [])] [];
  t "class a extends b, c { }" [] [Class(false, "a", ["b"; "c"], [], [])] [];
  t "class a extends b, c, d { }" [] [Class(false, "a", ["b"; "c"; "d"], [], [])] [];
  t "public class a extends b, c, d { }" [] [Class(true, "a", ["b"; "c"; "d"], [], [])] [];
  t "class a { b; }" [] [Class(false, "a", [], [], [es b])] [];
  t "class a { b; c; }" [] [Class(false, "a", [], [], [es b; es c])] [];
  t "class a { fun b c(){} }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [])] [];
  t "class a { fun b c(){} d; }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [es d])] [];
  t "class a { d; fun b c(){} }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [es d])] [];
  t "class a { fun b c(){} d; e; }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [es d; es e])] [];
  t "class a { d; fun b c(){} e; }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [es d; es e])] [];
  t "class a { d; e; fun b c(){} }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], [])], [es d; es e])] [];

  (* fair-weather mixtures *)
  t "a; b;" [] [] [es a; es b];
  t "fun a b(){} fun c d(){}" [Function(Type("a", false, false, []), "b", [], []); Function(Type("c", false, false, []), "d", [], [])] [] [];
  t "class a { } class b { }" [] [Class(false, "a", [], [], []); Class(false, "b", [], [], [])] [];
  t "a; fun a b(){}" [Function(Type("a", false, false, []), "b", [], [])] [] [es a];
  t "fun a b(){} a;" [Function(Type("a", false, false, []), "b", [], [])] [] [es a];
  t "a; class a { }" [] [Class(false, "a", [], [], [])] [es a];
  t "class a { } a;" [] [Class(false, "a", [], [], [])] [es a];
  t "fun a b(){} class a { }" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [];
  t "class a { } fun a b(){}" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [];
  t "a; class a { } fun a b(){}" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "a; fun a b(){} class a { }" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "class a { } a; fun a b(){}" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "fun a b(){} a; class a { }" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "class a { } fun a b(){} a;" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "fun a b(){} class a { } a;" [Function(Type("a", false, false, []), "b", [], [])] [Class(false, "a", [], [], [])] [es a];
  t "class a { fun b c(){} fun d e(){} }" [] [Class(false, "a", [], [Function(Type("b", false, false, []), "c", [], []); Function(Type("d", false, false, []), "e", [], [])], [])] [];

  (* little precedence things *)
  t "a + b * c;" [] [] [es (BinaryExpr(Plus, a, BinaryExpr(Multiply, b, c)))];
  t "b * c + a;" [] [] [es (BinaryExpr(Plus, BinaryExpr(Multiply, b, c), a))];
  t "a.b();" [] [] [es (FunCall(Member(a, "b"), []))];
  t "-a();" [] [] [es (UnaryExpr(ArithNegation, FunCall(a, [])))];

  print_endline ("failed " ^ (string_of_int !num_failed) ^ " of " ^ (string_of_int !num_tests))
