type ty = BoolTy | IntTy | UnitTy | AtomTy
let string_of_ty = function
  | BoolTy -> "Bool"
  | IntTy -> "Int"
  | UnitTy -> "Unit"
  | AtomTy -> "Atom"
