open Types
open L02ast

exception TypeError of string
exception UniqueError of string

let tyOf : 'a exp -> 'a = function
  | BoolLit (_, a) | IntLit (_, a) | AtomLit (_, a) | Var (_, a) -> a
  | UnitLit a -> a
  | Mathop (_, _, _, a) -> a | Cmpop (_, _, _, a) -> a | Not (_, a) -> a
  | IfElse (_, _, _, a) | Let (_, _, _, a) -> a
  | Fun (_, _, _, a) -> a | App (_, _, a) -> a

let tyMismatch : string -> ty -> ty -> 'a = fun what exp act ->
  raise @@ TypeError ("Type mismatch in " ^ what ^ ": expected "
  ^ string_of_ty exp ^ ", but got " ^ string_of_ty act ^ " instead")

let rec typecheck varenv (exp : unit exp) : ty exp =
  let ty = typecheck varenv in
  let rec assertNoDups = function
    | [] -> ()
    | x::xs -> if List.mem x xs then raise (UniqueError x) else assertNoDups xs
  in
  match exp with
  | BoolLit (b, _) -> BoolLit (b, BoolTy)
  | IntLit (i, _) -> IntLit (i, IntTy)
  | UnitLit _ -> UnitLit UnitTy
  | AtomLit (at, _) -> AtomLit (at, AtomTy)
  | Var (n, _) ->
      let tyN =
        try List.assoc n varenv
        with Not_found ->
          raise @@ TypeError ("Undeclared variable `" ^ n ^ "'")
      in Var (n, tyN)
  | Mathop (op, e1, e2, _) ->
      let opStr = List.assoc op [Plus,"+"; Minus,"-"; Times,"*"; Divide,"/"] in
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Mathop (op, t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch opStr IntTy q)
  | Cmpop (Equals, e1, e2, _) ->
      let (t1, t2) = (ty e1, ty e2) in
      if tyOf t1 <> tyOf t2 then tyMismatch "=" (tyOf t1) (tyOf t2);
      Cmpop (Equals, t1, t2, BoolTy)
  | Cmpop (And, e1, e2, _) ->
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (BoolTy, BoolTy) -> Cmpop (And, t1, t2, BoolTy)
      | (BoolTy, q) | (q, BoolTy) | (q, _) -> tyMismatch "and" BoolTy q)
  | Cmpop (Or, e1, e2, _) ->
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (BoolTy, BoolTy) -> Cmpop (Or, t1, t2, BoolTy)
      | (BoolTy, q) | (q, BoolTy) | (q, _) -> tyMismatch "or" BoolTy q)
  | Cmpop (op, e1, e2, _) ->
      let opStr = List.assoc op [Lt,"<"; Lte,"<="; Gt,">"; Gte,">="] in
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Cmpop (op, t1, t2, BoolTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch opStr IntTy q)
  | Not (e, _) ->
      let te = ty e in
      if tyOf te <> BoolTy then tyMismatch "not" BoolTy (tyOf te);
      Not (te, BoolTy)
  | IfElse (cond, ift, iff, _) ->
      let tcond = ty cond in
      if tyOf tcond <> BoolTy then tyMismatch "if" BoolTy (tyOf tcond);
      let (tift, tiff) = (ty ift, ty iff) in
      if tyOf tift <> tyOf tiff then tyMismatch "if" (tyOf tift) (tyOf tiff);
      IfElse (tcond, tift, tiff, tyOf tift)
  (* TODO: cleanup *)
  | Let ((n, t), (Fun (formals, fret, fbody, _) as e), b, _) ->
      let tyFormals = List.map snd formals in
      let tyN = FunTy (tyFormals, fret) in
      let te = typecheck ((n,tyN)::varenv) e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let ((n, t), te, tb, tyOf tb)
  | Let ((n, t), e, b, _) ->
      let te = ty e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let ((n, t), te, tb, tyOf tb)
  | Fun (formals, ty, body, _) ->
      let formalNames = List.map fst formals in
      let () = assertNoDups formalNames in
      let tyFormals = List.map snd formals in
      let varenv' = formals @ varenv in
      let tbody = typecheck varenv' body in
      if tyOf tbody <> ty then tyMismatch "fun" ty (tyOf tbody);
      Fun (formals, ty, tbody, FunTy (tyFormals, ty))
  | App (f, actuals, _) ->
      let typedActuals = List.map ty actuals in
      let typesOfActuals = List.map tyOf typedActuals in
      let tyF = ty f in
      (match tyOf tyF with
      | FunTy (typesOfFormals, retTy) ->
          if typesOfActuals <> typesOfFormals
          then tyMismatch "apply" (tyOf tyF) (FunTy (typesOfActuals, retTy))
          else App (tyF, typedActuals, retTy)
      | _ -> raise @@ TypeError "Non-function applied to arguments")
