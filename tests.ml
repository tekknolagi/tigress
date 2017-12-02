module LEX = L00lexer
module PAR = L01parser
module AST = L02ast
module TYP = L03typecheck
module SCO = L04scoperesolution
module EVA = L05eval
module LOW = L05mir

let exp s = s ^ ";"
let paren s = "(" ^ s ^ ")"

let i x = AST.IntLit (x, ())
let a x = AST.AtomLit (x, ())
let v x = AST.Var (x, ())
let u = AST.UnitLit ()

let parse_expressions =
  let open AST in
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
  "1+2 < 3", Cmpop (Lt, Mathop (Plus, i 1, i 2, ()), i 3, ());
  "1 + (2 < 3)", Mathop (Plus, i 1, Cmpop (Lt, i 2, i 3, ()), ());
  "if 3 then 4 else 5", IfElse (i 3, i 4, i 5, ());
  "if 5 < 6 then 4 else 5", IfElse (Cmpop (Lt, i 5, i 6, ()), i 4, i 5, ());
  "if 5 < 6 then a else b",
    IfElse (Cmpop (Lt, i 5, i 6, ()), a "a", a "b", ());
  "if 3 then 4 end", IfElse (i 3, i 4, u, ());
  "let X : Int = 5 in X", Let (("X", IntTy), i 5, v "X", ());
  "let X : Int = 5 in X + 1",
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
  "let F = \\(X:Int):Int = 1 in F(hello)",
    Let (
      ("F", FunTy ([IntTy], IntTy)),
      Fun (["X", IntTy], IntTy, i 1, ()),
      App (v "F", [a "hello"], ()),
      ());
  "let F : (Int) -> Int = Fact in F",
    Let (("F", FunTy ([IntTy], IntTy)), v "Fact", v "F", ());
  "let F : Int -> Int = Fact in F",
    Let (("F", FunTy ([IntTy], IntTy)), v "Fact", v "F", ());
  "let F : ((Bool * Int) -> Atom) -> Int = Fact in F",
    Let (("F", FunTy ([FunTy ([BoolTy; IntTy], AtomTy)], IntTy)), v "Fact", v "F", ());
]

let parse_tests =
  List.map (fun (s,a) -> exp s, a) parse_expressions
  @ List.map (fun (s,a) -> exp (paren s), a) parse_expressions


let typesafe_expressions = [
  "1";
  "1 + 2";
  "let F : Int = 3 in F";
  "let F : Int = 3 in F + 1";
  "(\\(X:Int):Int = X * 3)(2)";
  "let Fact = \\(X:Int):Int = " ^
  "  if X < 2 then 1 else X*Fact(X-1)" ^
  "in let F : Int -> Int = Fact in F(5)";
  "let Fact(X:Int):Int = " ^
  "  if X < 2 then 1 else X*Fact(X-1)" ^
  "in let F : Int -> Int = Fact in F(5)";
]

let typesafe_tests =
  List.map exp typesafe_expressions


let not_typesafe_expressions = [
  "1 + hello";
  "1 + true";
  "F";
  "let F : Int = 3 in F(3)";
  "(\\(X:Int):Int = X * 3)(hello)";
]

let not_typesafe_tests =
  List.map exp not_typesafe_expressions


let typed_expressions =
  let open AST in
  let open Types in
[
  "1", IntLit (1, IntTy);
  "\\(X:Int):Int = X",
    Fun (["X", IntTy], IntTy, Var ("X", IntTy), FunTy ([IntTy], IntTy));
]

let typed_tests =
  List.map (fun (s,a) -> exp s, a) typed_expressions


let renaming_tests = []


let eval_expressions =
  let open AST in
[
  "1", IntVal 1;
  "1 + 2", IntVal 3;
  "let Fact = \\(X:Int):Int = " ^
  "  if X < 2 then 1 else X*Fact(X-1)" ^
  "in Fact(5)", IntVal 120;
  "let Fact(X:Int):Int = " ^
  "  if X < 2 then 1 else X*Fact(X-1)" ^
  "in Fact(5)", IntVal 120;
]

let eval_tests =
  List.map (fun (s,a) -> exp s, a) eval_expressions


(* TODO: Figure out a nicer way to do this. *)
let lower_expressions =
  let open AST in
  let open LOW in
  let gen_main ins =
    Fun ({
      fundecl = ("main", [], Types.FunTy ([], Types.UnitTy));
      impl = ins;
    })
  in
[
  "1", [gen_main [Ret (Imm 1)]];
  "1+2", [gen_main [ Ret (Binop (Math Plus, Imm 1, Imm 2)) ]];
  "1<2", [gen_main [ Ret (Binop (Cmp Lt, Imm 1, Imm 2)) ]];
]

let lower_tests =
  List.map (fun (s,a) -> exp s, a) lower_expressions


exception DidNotParse of string
exception ShouldNotHavePassed of string


let () =
  let parse s =
    try PAR.main LEX.token @@ Lexing.from_string s
    with exc -> raise @@ DidNotParse s
  in
  let _type = TYP.typecheck [] in
  let rename = SCO.rename [] in
  let eval  = EVA.eval [] in
  let lower = LOW.lower in

  let run_parse_test (given, expected) = assert ((parse given)=expected) in
  let run_typesafe_test given = ignore @@ _type @@ parse given in
  let run_not_typesafe_test given =
    try ( ignore @@ _type @@ parse given; raise @@ ShouldNotHavePassed given )
    with TYP.TypeError _ -> ()
  in
  let run_typed_test (given, expected) =
    assert ((_type @@ parse @@ given)=expected)
  in
  let run_renaming_test (given, expected) =
    assert true
  in
  let run_eval_test (given, expected) =
    assert ((eval @@ rename @@ _type @@ parse @@ given)=expected)
  in
  let run_lower_test (given, expected) =
    assert ((lower @@ rename @@ _type @@ parse @@ given)=expected)
  in

  let indent s = print_string @@ "  " ^ s in
  let run_test_suite what f tests = (
    indent @@ "Running " ^ what ^ " tests...";
    List.iter f tests;
    print_endline @@ " " ^ (string_of_int @@ List.length tests) ^ " passed!";
  )
  in (
    print_endline "Running tests...";

    run_test_suite "parse" run_parse_test parse_tests;
    run_test_suite "type-safety" run_typesafe_test typesafe_tests;
    run_test_suite "non-type-safety" run_not_typesafe_test not_typesafe_tests;
    run_test_suite "type annotation" run_typed_test typed_tests;
    run_test_suite "renaming" run_renaming_test renaming_tests;
    run_test_suite "eval" run_eval_test eval_tests;
    run_test_suite "lower" run_lower_test lower_tests;

    print_endline "All tests passed.";
  )
