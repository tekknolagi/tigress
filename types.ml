open Common

type ty = BoolTy | IntTy | UnitTy | AtomTy | FunTy of (ty list * ty)
        | T of
string
let rec string_of_ty = function
  | BoolTy -> "Bool"
  | IntTy -> "Int"
  | UnitTy -> "Unit"
  | AtomTy -> "Atom"
  | FunTy ([], ret) -> "(nothing) -> " ^ string_of_ty ret
  | FunTy (args, ret) ->
      S.map_concat ", " string_of_ty args ^ " -> " ^ string_of_ty ret
  | T n -> "'" ^ n

type 'a env = (string * 'a) list

type renamed = [ `Renamed ]
