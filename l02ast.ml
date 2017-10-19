type exp =
  | BoolLit of bool
  | IntLit of int
  | Plus of (exp * exp)
  | Minus of (exp * exp)
  | Times of (exp * exp)
  | Divide of (exp * exp)

let rec string_of_exp = function
  | BoolLit b -> "BoolLit " ^ string_of_bool b
  | IntLit i -> "IntLit " ^ string_of_int i
  | Plus (e1, e2) ->
    "Plus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Minus (e1, e2) ->
    "Minus (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Times (e1, e2) ->
    "Times (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Divide (e1, e2) ->
    "Divide (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
