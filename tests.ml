let exp s = s ^ ";"
let paren s = "(" ^ s ^ ")"

let i x = L02ast.IntLit (x, ())
let a x = L02ast.AtomLit (x, ())
let v x = L02ast.Var (x, ())
let u = L02ast.UnitLit ()

let parse_expressions =
  let open L02ast in
  let open Types in
[
  "true", BoolLit (true, ());
  "false", BoolLit (false, ());
  "1", i 1;
  "100", i 100;
  "-12", Mathop (Minus, i 0, i 12, ());
  "- 12", Mathop (Minus, i 0, i 12, ());
  "100+2", Mathop (Plus, i 100, i 2, ());
  "100 + 2", Mathop (Plus, i 100, i 2, ());
  "100-2", Mathop (Minus, i 100, i 2, ());
  "100*2", Mathop (Times, i 100, i 2, ());
  "100/2", Mathop (Divide, i 100, i 2, ());
  "atom", AtomLit ("atom", ());
  "atom_with_underscores", AtomLit ("atom_with_underscores", ());
  "V", Var ("V", ());
  "Var", Var ("Var", ());
  "()", UnitLit ();
  "1=2", Cmpop (Equals, i 1, i 2, ());
  "1<2", Cmpop (Lt, i 1, i 2, ());
  "1<=2", Cmpop (Lte, i 1, i 2, ());
  "1>2", Cmpop (Gt, i 1, i 2, ());
  "1>=2", Cmpop (Gte, i 1, i 2, ());
  "1<>2", Not (Cmpop (Equals, i 1, i 2, ()), ());
  "if 3 then 4 else 5 end", IfElse (i 3, i 4, i 5, ());
  "if 5 < 6 then 4 else 5 end", IfElse (Cmpop (Lt, i 5, i 6, ()), i 4, i 5, ());
  "if 5 < 6 then a else b end",
    IfElse (Cmpop (Lt, i 5, i 6, ()), a "a", a "b", ());
  "if 3 then 4 end", IfElse (i 3, i 4, u, ());
  "let X : Int = 5 in X end", Let (("X", IntTy), i 5, v "X", ());
  "let X : Int = 5 in X + 1 end",
    Let (("X", IntTy), i 5, Mathop (Plus, v "X", i 1, ()), ());
  "fun (X:Int):Int = X + 1",
    Fun (["X", IntTy], IntTy, Mathop (Plus, v "X", i 1, ()), ());
  "\\(X:Int):Int = X + 1",
    Fun (["X", IntTy], IntTy, Mathop (Plus, v "X", i 1, ()), ());
  "F(3)", App (v "F", [i 3], ());
  "(F)(3)", App (v "F", [i 3], ());
  "(\\(X:Int):Int = X + 1)(3)",
    App (
      Fun (["X", IntTy], IntTy, Mathop (Plus, v "X", i 1, ()), ()),
      [i 3],
      ());
  "(\\(X:Int):Int = X + 1)(3, 4)",
    App (
      Fun (["X", IntTy], IntTy, Mathop (Plus, v "X", i 1, ()), ()),
      [i 3; i 4],
      ());
  "let F = \\(X:Int):Int = 1 in F(hello) end",
    Let (
      ("F", FunTy ([IntTy], IntTy)),
      Fun (["X", IntTy], IntTy, i 1, ()),
      App (v "F", [a "hello"], ()),
      ());
  "let F : (Int) -> Int = Fact in F end",
    Let (("F", FunTy ([IntTy], IntTy)), v "Fact", v "F", ());
  "let F : Int -> Int = Fact in F end",
    Let (("F", FunTy ([IntTy], IntTy)), v "Fact", v "F", ());
  "let F : ((Bool * Int) -> Atom) -> Int = Fact in F end",
    Let (("F", FunTy ([FunTy ([BoolTy; IntTy], AtomTy)], IntTy)), v "Fact", v "F", ());
]

let parse_tests =
  List.map (fun (s,a) -> exp s, a) parse_expressions
  @ List.map (fun (s,a) -> exp (paren s), a) parse_expressions


let typesafe_expressions = [
  "1";
  "1 + 2";
  "let F : Int = 3 in F end";
  "let F : Int = 3 in F + 1 end";
]

let typesafe_tests =
  List.map exp typesafe_expressions


let not_typesafe_expressions = [
  "1 + hello";
  "1 + true";
  "F";
  "let F : Int = 3 in F(3) end";
]

let not_typesafe_tests =
  List.map exp not_typesafe_expressions


let type_tests = let open L02ast in []


let eval_tests = let open L02ast in []


exception ShouldNotHavePassed


let () =
  let parse s = L01parser.main L00lexer.token @@ Lexing.from_string s in
  let _type a = L02ast.typecheck [] a in
  let eval  t = L03eval.eval [] t in

  let run_parse_test (given, expected) = assert ((parse given)=expected) in
  let run_typesafe_test given = ignore @@ _type @@ parse given in
  let run_not_typesafe_test given =
    try ( ignore @@ _type @@ parse given; raise ShouldNotHavePassed )
    with L02ast.TypeError _ -> ()
  in
  let run_type_test (given, expected)  = assert ((_type given)=expected) in
  let run_eval_test (given, expected)  = assert ((eval given)=expected) in

  List.iter run_parse_test parse_tests;
  List.iter run_typesafe_test typesafe_tests;
  List.iter run_not_typesafe_test not_typesafe_tests;
  List.iter run_type_test  type_tests;
  List.iter run_eval_test  eval_tests
