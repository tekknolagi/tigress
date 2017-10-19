type exp =
  | BoolLit of bool
  | IntLit of int
  | Plus of (exp * exp)
  | Minus of (exp * exp)
  | Times of (exp * exp)
  | Divide of (exp * exp)
