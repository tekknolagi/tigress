type ty = BoolTy | IntTy | UnitTy | AtomTy | FunTy of (ty list * ty)
let rec string_of_ty = function
  | BoolTy -> "Bool"
  | IntTy -> "Int"
  | UnitTy -> "Unit"
  | AtomTy -> "Atom"
  | FunTy (args, ret) ->
      (String.concat ", " @@ List.map string_of_ty args) ^ " -> "
      ^ string_of_ty ret

type value =
  | BoolVal of bool
  | IntVal of int
  | UnitVal
  | AtomVal of string
  (* | FunVal of  *)
let string_of_value = function
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | UnitVal -> "()"
  | AtomVal a -> a
