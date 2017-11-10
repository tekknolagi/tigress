exception BugInTypeChecking of string
exception DivByZero

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
        | (IntVal v1, IntVal v2) -> IntVal (v1+v2)
        | _ -> raise @@ BugInTypeChecking "Plus")
    | Minus (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> IntVal (v1-v2)
        | _ -> raise @@ BugInTypeChecking "Minus")
    | Times (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> IntVal (v1*v2)
        | _ -> raise @@ BugInTypeChecking "Times")
    | Divide (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal 0) -> raise @@ DivByZero
        | (IntVal v1, IntVal v2) -> IntVal (v1/v2)
        | _ -> raise @@ BugInTypeChecking "Divide")
    | Not (e, _) ->
        (match ev e with
        | BoolVal v -> BoolVal (not v)
        | _ -> raise @@ BugInTypeChecking "Not")
    | Lt (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 < v2)
        | _ -> raise @@ BugInTypeChecking "Lt")
    | Lte (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 <= v2)
        | _ -> raise @@ BugInTypeChecking "Lte")
    | Gt (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 > v2)
        | _ -> raise @@ BugInTypeChecking "Gt")
    | Gte (e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal v2) -> BoolVal (v1 >= v2)
        | _ -> raise @@ BugInTypeChecking "Gte")
    | Equals (e1, e2, _) -> BoolVal ((ev e1)=(ev e2))
    | IfElse (cond, iftrue, iffalse, _) ->
        (match ev cond with
        | BoolVal true -> ev iftrue
        | BoolVal false -> ev iffalse
        | _ -> raise @@ BugInTypeChecking "IfElse")
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
            eval varenv' body
        | _ -> raise @@ BugInTypeChecking "App")
  in ev exp
