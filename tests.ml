let exp s = s ^ ";"
let paren s = "(" ^ s ^ ")"

let i x = L02ast.IntLit (x, ())
let a x = L02ast.AtomLit (x, ())
let u = L02ast.UnitLit ()

let expressions = let open L02ast in [
  "true", BoolLit (true, ());
  "false", BoolLit (false, ());
  "1", i 1;
  "100", i 100;
  "-12", Mathop (Minus, i 0, i 12, ());
  "100+2", Mathop (Plus, i 100, i 2, ());
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
  "if 5 < 6 then a else b end", IfElse (Cmpop (Lt, i 5, i 6, ()), a "a", a "b", ());
  "if 3 then 4 end", IfElse (i 3, i 4, u, ());
]

let parse_tests =
  List.map (fun (s,a) -> exp s, a) expressions
  @ List.map (fun (s,a) -> exp (paren s), a) expressions

let type_tests = let open L02ast in []

let eval_tests = let open L02ast in []

let () =
  let parse s = L01parser.main L00lexer.token @@ Lexing.from_string s in
  let _type a = L02ast.typecheck [] a in
  let eval  t = L03eval.eval [] t in

  let run_parse_test (given, expected) =
    assert ((parse given)=expected)
  in
  let run_type_test (given, expected) =
    assert ((_type given)=expected)
  in
  let run_eval_test (given, expected) =
    assert ((eval given)=expected)
  in

  List.iter run_parse_test parse_tests;
  List.iter run_type_test  type_tests;
  List.iter run_eval_test  eval_tests
