open Types

type 'a exp =
  | BoolLit of (bool * 'a)
  | IntLit of (int * 'a)
  | UnitLit of 'a
  | AtomLit of (name * 'a)
  | Var of (name * 'a)

  | Mathop of (mathop * 'a exp * 'a exp * 'a)
  | Cmpop of (cmpop * 'a exp * 'a exp * 'a)
  | Not of ('a exp * 'a)

  | IfElse of ('a exp * 'a exp * 'a exp * 'a)
  | Let of (vardecl * 'a exp * 'a exp * 'a)

  | Fun of (vardecl list * ty * 'a exp * 'a)
  | App of ('a exp * 'a exp list * 'a)

and mathop = Plus | Minus | Times | Divide
and cmpop = Equals | Lt | Lte | Gt | Gte

and vardecl = name * ty
and name = string

and 'a value =
  | BoolVal of bool
  | IntVal of int
  | UnitVal
  | AtomVal of string
  | ClosureVal of (name list * 'a exp * 'a value ref env)
  | Unspecified
let string_of_value = function
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | UnitVal -> "()"
  | AtomVal a -> a
  | ClosureVal _ -> "(closure)"
  | Unspecified -> "UNSPECIFIED"

let string_of_vardecl (n, t) = n ^ " : " ^ string_of_ty t

let rec string_of_aexp f exp =
  let toS = string_of_aexp f in
  let ann a = let s = f a in if s="" then "" else " : " ^ s in
  let fmt2 tag l r a = tag ^ " (" ^ toS l ^ ", " ^ toS r ^ ")" ^ ann a in
  let string_of_mathop o =
    List.assoc o [Plus,"Plus"; Minus,"Minus"; Times,"Times"; Divide,"Divide"]
  in
  let string_of_cmpop o =
    List.assoc o [Lt,"Lt"; Lte,"Lte"; Gt,"Gt"; Gte,"Gte"; Equals,"Equals"]
  in
  match exp with
  | BoolLit (b, a) -> "BoolLit " ^ string_of_bool b ^ ann a
  | IntLit (i, a) -> "IntLit " ^ string_of_int i ^ ann a
  | UnitLit a -> "UnitLit" ^ ann a
  | AtomLit (at, a) -> "Atom " ^ at ^ ann a
  | Var (n, a) -> "Var " ^ n ^ ann a
  | Mathop (op, e1, e2, a) -> fmt2 (string_of_mathop op) e1 e2 a
  | Cmpop (op, e1, e2, a) -> fmt2 (string_of_cmpop op) e1 e2 a
  | Not (e, a) -> "Not " ^ toS e ^ ann a
  | IfElse (cond, ift, iff, a) ->
    "IfElse (" ^ toS cond ^ ", " ^ toS ift ^ ", " ^ toS iff ^ ")" ^ ann a
  | Let ((n, _), e, b, a) ->
    "Let (" ^ n ^ ", " ^ toS e ^ ", " ^ toS b ^ ")" ^ ann a
  | Fun (ns, t, b, a) ->
    "Fun ([" ^ (String.concat ", " @@ List.map string_of_vardecl ns) ^ "]"
    ^ ", " ^ toS b ^ ")" ^ ann a
  | App (fe, es, a) ->
    "App (" ^ toS fe ^ ", [" ^ (String.concat ", " @@ List.map toS es) ^ "]"
    ^ ")" ^ ann a
