let exp s = s ^ ";"
let paren s = "(" ^ s ^ ")"

let expressions = let open L02ast in [
  "true", BoolLit (true, ());
  "false", BoolLit (false, ());
  "1", IntLit (1, ());
  "100", IntLit (100, ());
  "-12", Mathop (Minus, IntLit (0, ()), IntLit (12, ()), ());
  "100+2", Mathop (Plus, IntLit (100, ()), IntLit (2, ()), ());
  "100-2", Mathop (Minus, IntLit (100, ()), IntLit (2, ()), ());
  "100*2", Mathop (Times, IntLit (100, ()), IntLit (2, ()), ());
  "100/2", Mathop (Divide, IntLit (100, ()), IntLit (2, ()), ());
  "atom", AtomLit ("atom", ());
  "atom_with_underscores", AtomLit ("atom_with_underscores", ());
  "V", Var ("V", ());
  "Var", Var ("Var", ());
  "()", UnitLit ();
  "1=2", Cmpop (Equals, IntLit (1, ()), IntLit (2, ()), ());
  "1<2", Cmpop (Lt, IntLit (1, ()), IntLit (2, ()), ());
  "1<=2", Cmpop (Lte, IntLit (1, ()), IntLit (2, ()), ());
  "1>2", Cmpop (Gt, IntLit (1, ()), IntLit (2, ()), ());
  "1>=2", Cmpop (Gte, IntLit (1, ()), IntLit (2, ()), ());
  "1<>2", Not (Cmpop (Equals, IntLit (1, ()), IntLit (2, ()), ()), ());
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
