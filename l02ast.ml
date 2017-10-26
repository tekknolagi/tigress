type 'a exp =
  | BoolLit of (bool * 'a)
  | IntLit of (int * 'a)
  | UnitLit of 'a
  | Atom of (name * 'a)
  | Var of (name * 'a)
  | Plus of ('a exp * 'a exp * 'a)
  | Minus of ('a exp * 'a exp * 'a)
  | Times of ('a exp * 'a exp * 'a)
  | Divide of ('a exp * 'a exp * 'a)
  | IfElse of ('a exp * 'a exp * 'a exp * 'a)
  | Let of (name * 'a exp * 'a exp * 'a)

and name = string

let rec string_of_exp = function
  | BoolLit (b, _) -> "BoolLit " ^ string_of_bool b
  | IntLit (i, _) -> "IntLit " ^ string_of_int i
  | UnitLit _ -> "UnitLit"
  | Atom (a, _) -> "Atom " ^ a
  | Var (n, _) -> "Var " ^ n
  | Plus (e1, e2, _) ->
    "Plus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Minus (e1, e2, _) ->
    "Minus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Times (e1, e2, _) ->
    "Times (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Divide (e1, e2, _) ->
    "Divide (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | IfElse (cond, ift, iff, _) ->
    "IfElse (" ^ string_of_exp cond ^ ", " ^ string_of_exp ift
    ^ ", " ^ string_of_exp iff ^ ")"
  | Let (n, e, b, _) ->
    "Let (" ^ n ^ ", " ^ string_of_exp e ^ ", " ^ string_of_exp b ^ ")"
