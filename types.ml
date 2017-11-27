type ty = BoolTy | IntTy | UnitTy | AtomTy | FunTy of (ty list * ty)
let rec string_of_ty = function
  | BoolTy -> "Bool"
  | IntTy -> "Int"
  | UnitTy -> "Unit"
  | AtomTy -> "Atom"
  | FunTy ([], ret) -> "(nothing) -> " ^ string_of_ty ret
  | FunTy (args, ret) ->
      (String.concat ", " @@ List.map string_of_ty args) ^ " -> "
      ^ string_of_ty ret

type 'a env = (string * 'a) list

type renamed = [ `Renamed ]
