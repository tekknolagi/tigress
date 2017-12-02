open Common

type 'a value = 'a L02ast.value
type 'a env = 'a Types.env
type renamed = Types.renamed

exception BugInTypeChecking of string
exception DivByZero
exception Unspecified of string
exception Unbound of string


[@@@ warning "-8"] (* Ignore non-exhaustive *)
let rec eval (varenv : (renamed value ref) env) (exp : renamed L02ast.exp) =
  let open L02ast in
  let rec ev = function
    | BoolLit (b, _) -> BoolVal b
    | IntLit (i, _) -> IntVal i
    | UnitLit _ -> UnitVal
    | AtomLit (a, _) -> AtomVal a
    | Var (n, _) ->
        (match L.assoc_opt n varenv with
        | Some { contents = Unspecified } -> raise @@ Unspecified n
        | Some { contents = v } -> v
        | None -> raise @@ Unbound n)
    | Mathop (Divide, e1, e2, _) ->
        (match (ev e1, ev e2) with
        | (IntVal v1, IntVal 0) -> raise @@ DivByZero
        | (IntVal v1, IntVal v2) -> IntVal (v1/v2))
    | Mathop (op, e1, e2, _) ->
        let opfn = List.assoc op [Plus, (+); Minus, (-); Times, ( * )] in
        let (IntVal v1, IntVal v2) = (ev e1, ev e2) in IntVal (opfn v1 v2)
    | Cmpop (Equals, e1, e2, _) -> BoolVal ((ev e1)=(ev e2))
    | Cmpop (And, e1, e2, _) ->
        let BoolVal v1 = ev e1 in
        if v1 then ev e2
        else BoolVal v1
    | Cmpop (Or, e1, e2, _) ->
        let BoolVal v1 = ev e1 in
        if not v1 then ev e2
        else BoolVal v1
    | Cmpop (op, e1, e2, _) ->
        let opfn = List.assoc op [Lt, (<); Lte, (<=); Gt, (>); Gte, (>=)] in
        let (IntVal v1, IntVal v2) = (ev e1, ev e2) in BoolVal (opfn v1 v2)
    | Not (e, _) -> let BoolVal v = ev e in BoolVal (not v)
    | IfElse (cond, iftrue, iffalse, _) ->
        let BoolVal v = ev cond in if v then ev iftrue else ev iffalse
    | Let ((n, _), (Fun _ as e), b, _) ->
        let ClosureVal (formalNames, body, cl_env) = ev e in
        let rec v' = ClosureVal (formalNames, body, (n, ref v')::cl_env) in
        let varenv' = (n, ref v')::varenv in
        eval varenv' b
    | Let ((n, _), e, b, _) -> eval ((n, ref @@ ev e)::varenv) b
    | Fun (formals, _, body, _) ->
        ClosureVal (List.map fst formals, body, varenv)
    | App (f, actuals, _) ->
        let actualsv = List.map ref @@ List.map ev actuals in
        let ClosureVal (formals, body, clenv) = ev f in
        eval ((List.combine formals actualsv) @ clenv) body
  in
    try ev exp
    with Match_failure _ -> raise @@ BugInTypeChecking "Somewhere"
