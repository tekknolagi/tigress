open Types

type 'a exp =
  | BoolLit of (bool * 'a)
  | IntLit of (int * 'a)
  | UnitLit of 'a
  | AtomLit of (name * 'a)
  | Var of (name * 'a)

  | Plus of ('a exp * 'a exp * 'a)
  | Minus of ('a exp * 'a exp * 'a)
  | Times of ('a exp * 'a exp * 'a)
  | Divide of ('a exp * 'a exp * 'a)

  | Not of ('a exp * 'a)
  | Equals of ('a exp * 'a exp * 'a)
  | Lt of ('a exp * 'a exp * 'a)
  | Lte of ('a exp * 'a exp * 'a)
  | Gt of ('a exp * 'a exp * 'a)
  | Gte of ('a exp * 'a exp * 'a)

  | IfElse of ('a exp * 'a exp * 'a exp * 'a)
  | Let of (vardecl * 'a exp * 'a exp * 'a)
  | Fun of (vardecl list * ty * 'a exp * 'a)

and vardecl = name * ty

and name = string

(*
let default_mapper mapper exp =
  exp

let f mapper exp =
  match exp with
  | IntLit (i, a) -> IntLit (i*2, a)
  | _ -> exp

let map_ast f exp =
  match exp with
  | BoolLit (b, a) -> 
      *)

let rec string_of_aexp f exp =
  let toS = string_of_aexp f in
  let fmt2 tag l r a = tag ^ " (" ^ toS l ^ ", " ^ toS r ^ ") : " ^ f a in
  let string_of_vardecl (n, t) = n ^ " : " ^ string_of_ty t in
  match exp with
  | BoolLit (b, a) -> "BoolLit " ^ string_of_bool b ^ " : " ^ f a
  | IntLit (i, a) -> "IntLit " ^ string_of_int i ^ " : " ^ f a
  | UnitLit a -> "UnitLit : " ^ f a
  | AtomLit (at, a) -> "Atom " ^ at ^ " : " ^ f a
  | Var (n, a) -> "Var " ^ n ^ " : " ^ f a
  | Plus (e1, e2, a) -> fmt2 "Plus" e1 e2 a
  | Minus (e1, e2, a) -> fmt2 "Minus" e1 e2 a
  | Times (e1, e2, a) -> fmt2 "Times" e1 e2 a
  | Divide (e1, e2, a) -> fmt2 "Divide" e1 e2 a
  | Not (e, a) -> "Not " ^ toS e ^ " : " ^ f a
  | Equals (e1, e2, a) -> fmt2 "Equals" e1 e2 a
  | Lt (e1, e2, a) -> fmt2 "Lt" e1 e2 a
  | Lte (e1, e2, a) -> fmt2 "Lte" e1 e2 a
  | Gt (e1, e2, a) -> fmt2 "Gt" e1 e2 a
  | Gte (e1, e2, a) -> fmt2 "Gte" e1 e2 a
  | IfElse (cond, ift, iff, a) ->
    "IfElse (" ^ toS cond ^ ", " ^ toS ift ^ ", " ^ toS iff ^ ") : " ^ f a
  | Let ((n, _), e, b, a) ->
    "Let (" ^ n ^ ", " ^ toS e ^ ", " ^ toS b ^ ") : " ^ f a
  | Fun (ns, t, b, a) ->
    "Fun ([" ^ (String.concat ", " @@ List.map string_of_vardecl ns) ^ "]"
    ^ ", " ^ toS b ^ ") : " ^ f a


exception TypeError of string

let tyOf = function
  | BoolLit (_, a) | IntLit (_, a) | AtomLit (_, a) | Var (_, a) -> a
  | UnitLit a -> a
  | Plus (_, _, a) | Minus (_, _, a) | Times (_, _, a) | Divide (_, _, a) -> a
  | Not (_, a) -> a
  | Equals (_, _, a) -> a
  | Lt (_, _, a) | Lte (_, _, a) | Gt (_, _, a) | Gte (_, _, a) -> a
  | IfElse (_, _, _, a) | Let (_, _, _, a) -> a
  | Fun (_, _, _, a) -> a | App (_, _, a) -> a

let tyMismatch what exp act =
  raise @@ TypeError ("Type mismatch in " ^ what ^ ": expected "
  ^ string_of_ty exp ^ ", but got " ^ string_of_ty act ^ " instead")

let rec typecheck varenv exp =
  let ty = typecheck varenv in
  let checkMath what e1 e2 =
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> (t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch what IntTy q)
  in
  let checkRel what e1 e2 =
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> (t1, t2, BoolTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch what IntTy q)
  in
  match exp with
  | BoolLit (b, a) -> BoolLit (b, BoolTy)
  | IntLit (i, a) -> IntLit (i, IntTy)
  | UnitLit a -> UnitLit UnitTy
  | AtomLit (at, a) -> AtomLit (at, AtomTy)
  | Var (n, a) -> Var (n, List.assoc n varenv)
  | Plus (e1, e2, a) -> Plus (checkMath "+" e1 e2)
  | Minus (e1, e2, a) -> Minus (checkMath "-" e1 e2)
  | Times (e1, e2, a) -> Times (checkMath "*" e1 e2)
  | Divide (e1, e2, a) -> Divide (checkMath "/" e1 e2)
  | Not (e, a) ->
      let te = ty e in
      if tyOf te <> BoolTy then tyMismatch "not" BoolTy (tyOf te);
      Not (te, BoolTy)
  | Lt (e1, e2, a) -> Lt (checkRel "<" e1 e2)
  | Lte (e1, e2, a) -> Lte (checkRel "<" e1 e2)
  | Gt (e1, e2, a) -> Gt (checkRel "<" e1 e2)
  | Gte (e1, e2, a) -> Gte (checkRel "<" e1 e2)
  | Equals (e1, e2, a) ->
      let (t1, t2) = (ty e1, ty e2) in
      if tyOf t1 <> tyOf t2 then tyMismatch "=" (tyOf t1) (tyOf t2);
      Equals (t1, t2, BoolTy)
  | IfElse (cond, ift, iff, a) ->
      let tcond = ty cond in
      if tyOf tcond <> BoolTy then tyMismatch "if" BoolTy (tyOf tcond);
      let (tift, tiff) = (ty ift, ty iff) in
      if tyOf tift <> tyOf tiff then tyMismatch "if" (tyOf tift) (tyOf tiff);
      IfElse (tcond, tift, tiff, tyOf tift)
  | Let ((n, t), e, b, a) ->
      let te = ty e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let ((n, t), te, tb, tyOf tb)
  | Fun (formals, ty, body, a) ->
      let varenv' = formals @ varenv in
      let tbody = typecheck varenv' body in
      if tyOf tbody <> ty then tyMismatch "fun" ty (tyOf tbody);
      Fun (formals, ty, tbody, ty)
