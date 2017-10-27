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
  | IfElse of ('a exp * 'a exp * 'a exp * 'a)
  | Let of (name * ty * 'a exp * 'a exp * 'a)

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

let rec string_of_exp exp = 
  let toS = string_of_exp in
  match exp with
  | BoolLit (b, _) -> "BoolLit " ^ string_of_bool b
  | IntLit (i, _) -> "IntLit " ^ string_of_int i
  | UnitLit _ -> "UnitLit"
  | AtomLit (a, _) -> "Atom " ^ a
  | Var (n, _) -> "Var " ^ n
  | Plus (e1, e2, _) -> "Plus (" ^ toS e1 ^ ", " ^ toS e2 ^ ")"
  | Minus (e1, e2, _) -> "Minus (" ^ toS e1 ^ ", " ^ toS e2 ^ ")"
  | Times (e1, e2, _) -> "Times (" ^ toS e1 ^ ", " ^ toS e2 ^ ")"
  | Divide (e1, e2, _) -> "Divide (" ^ toS e1 ^ ", " ^ toS e2 ^ ")"
  | IfElse (cond, ift, iff, _) ->
    "IfElse (" ^ toS cond ^ ", " ^ toS ift ^ ", " ^ toS iff ^ ")"
  | Let (n, t, e, b, _) -> "Let (" ^ n ^ " : " ^ string_of_ty t
    ^ ", " ^ toS e ^ ", " ^ toS b ^ ")"

let rec string_of_aexp f exp =
  let toS = string_of_aexp f in
  match exp with
  | BoolLit (b, a) -> "BoolLit " ^ string_of_bool b ^ " : " ^ f a
  | IntLit (i, a) -> "IntLit " ^ string_of_int i ^ " : " ^ f a
  | UnitLit a -> "UnitLit : " ^ f a
  | AtomLit (at, a) -> "Atom " ^ at ^ " : " ^ f a
  | Var (n, a) -> "Var " ^ n ^ " : " ^ f a
  | Plus (e1, e2, a) -> "Plus (" ^ toS e1 ^ ", " ^ toS e2 ^ ") : " ^ f a
  | Minus (e1, e2, a) -> "Minus (" ^ toS e1 ^ ", " ^ toS e2 ^ ") : " ^ f a
  | Times (e1, e2, a) -> "Times (" ^ toS e1 ^ ", " ^ toS e2 ^ ") : " ^ f a
  | Divide (e1, e2, a) -> "Divide (" ^ toS e1 ^ ", " ^ toS e2 ^ ") : " ^ f a
  | IfElse (cond, ift, iff, a) ->
    "IfElse (" ^ toS cond ^ ", " ^ toS ift ^ ", " ^ toS iff ^ ") : " ^ f a
  | Let (n, _, e, b, a) ->
    "Let (" ^ n ^ ", " ^ toS e ^ ", " ^ toS b ^ ") : " ^ f a


exception TypeError of string

let tyOf = function
  | BoolLit (_, a) | IntLit (_, a) | AtomLit (_, a) | Var (_, a) -> a
  | UnitLit a -> a
  | Plus (_, _, a) | Minus (_, _, a) | Times (_, _, a) | Divide (_, _, a) -> a
  | IfElse (_, _, _, a) | Let (_, _, _, _, a) -> a

let tyMismatch what exp act =
  raise @@ TypeError ("Type mismatch in " ^ what ^ ": expected "
  ^ string_of_ty exp ^ ", but got " ^ string_of_ty act ^ " instead")

let rec typecheck varenv = function
  | BoolLit (b, a) -> BoolLit (b, BoolTy)
  | IntLit (i, a) -> IntLit (i, IntTy)
  | UnitLit a -> UnitLit UnitTy
  | AtomLit (at, a) -> AtomLit (at, AtomTy)
  | Var (n, a) -> Var (n, List.assoc n varenv)
  | Plus (e1, e2, a) ->
      let (t1, t2) = (typecheck varenv e1, typecheck varenv e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Plus (t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch "+" IntTy q)
  | Minus (e1, e2, a) ->
      let (t1, t2) = (typecheck varenv e1, typecheck varenv e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Minus (t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch "-" IntTy q)
  | Times (e1, e2, a) ->
      let (t1, t2) = (typecheck varenv e1, typecheck varenv e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Times (t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch "*" IntTy q)
  | Divide (e1, e2, a) ->
      let (t1, t2) = (typecheck varenv e1, typecheck varenv e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Divide (t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch "/" IntTy q)
  | IfElse (cond, ift, iff, a) ->
      let tcond = typecheck varenv cond in
      if tyOf tcond <> BoolTy then tyMismatch "if" BoolTy (tyOf tcond);
      let (tift, tiff) = (typecheck varenv ift, typecheck varenv iff) in
      (match (tyOf tift, tyOf tiff) with
      | (t1, t2) when t1=t2 -> IfElse (tcond, tift, tiff, tyOf tift)
      | (t1, t2) -> tyMismatch "if" t1 t2)
  | Let (n, t, e, b, a) ->
      let te = typecheck varenv e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let (n, t, te, tb, tyOf tb)
