type exp =
  | BoolLit of bool
  | IntLit of int
  | UnitLit
  | Plus of (exp * exp)
  | Minus of (exp * exp)
  | Times of (exp * exp)
  | Divide of (exp * exp)
  | IfElse of (exp * exp * exp)

let rec string_of_exp = function
  | BoolLit b -> "BoolLit " ^ string_of_bool b
  | IntLit i -> "IntLit " ^ string_of_int i
  | UnitLit -> "UnitLit"
  | Plus (e1, e2) ->
    "Plus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Minus (e1, e2) ->
    "Minus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Times (e1, e2) ->
    "Times (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Divide (e1, e2) ->
    "Divide (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | IfElse (cond, ift, iff) ->
    "IfElse (" ^ string_of_exp cond ^ ", "
    ^ string_of_exp ift ^ ", " ^ string_of_exp iff ^ ")"
