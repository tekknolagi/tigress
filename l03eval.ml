exception BugInTypeChecking of string
exception DivByZero

[@@@ warning "-8"] (* Ignore non-exhaustive *)
let rec eval varenv (exp : Types.ty L02ast.exp) =
  let open L02ast in
  let rec ev = function
    | BoolLit (b, _) -> BoolVal b
    | IntLit (i, _) -> IntVal i
    | UnitLit _ -> UnitVal
    | AtomLit (a, _) -> AtomVal a
    | Var (n, _) -> List.assoc n varenv
    | Plus (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> IntVal (v1+v2))
    | Minus (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> IntVal (v1-v2))
    | Times (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> IntVal (v1*v2))
    | Divide (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal 0) -> raise @@ DivByZero
        | (IntVal v1, IntVal v2) -> IntVal (v1/v2))
    | Not (e, _) ->
        (match ev e with
        | BoolVal v -> BoolVal (not v))
    | Lt (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 < v2))
    | Lte (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 <= v2))
    | Gt (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 > v2))
    | Gte (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 >= v2))
    | Equals (e1, e2, _) -> BoolVal ((ev e1)=(ev e2))
    | IfElse (cond, iftrue, iffalse, _) ->
        (match ev cond with
        | BoolVal true -> ev iftrue
        | BoolVal false -> ev iffalse)
    | Let ((n, _), e, b, _) ->
        let varenv' = (n, ev e)::varenv in
        eval varenv' b
    (* TODO: handle recursion and stuff *)
    | Fun (formals, _, body, _) -> ClosureVal (List.map fst formals, body)
    | App (f, actuals, _) ->
        let actualsv = List.map ev actuals in
        (match ev f with
        | ClosureVal (formals, body) ->
            let varenv' = (List.combine formals actualsv) @ varenv in
            eval varenv' body)
  in
  try ev exp
  with Match_failure _ -> raise @@ BugInTypeChecking "Somewhere"
