open Types
open L02ast

exception BugInRenaming of string
type unique_name = name * int
let string_of_unique_name (n, c) =
  match c with
  | 0 -> n
  | _ -> "__" ^ n ^ string_of_int c

[@@@ warning "-8"] (* Ignore non-exhaustive *)
let rec rename (varenv : unique_name env) (exp : ty exp) : renamed exp =
  let re = rename varenv in
  let r = `Renamed in
  let is_being_shadowed = List.mem_assoc in
  let get_last_counter n h = snd @@ List.assoc n h in
  let gen_unique_name n =
    if is_being_shadowed n varenv
        then (n, 1 + get_last_counter n varenv)
        else (n, 0)
  in
  match exp with
  | BoolLit (b, _) -> BoolLit (b, r)
  | IntLit (i, _) -> IntLit (i, r)
  | UnitLit _ -> UnitLit r
  | AtomLit (a, _) -> AtomLit (a, r)
  | Var (n, _) ->
      let foundVariable = List.assoc n varenv in
      Var (string_of_unique_name foundVariable, r)
  | Mathop (op, e1, e2, _) -> Mathop (op, re e1, re e2, r)
  | Cmpop (op, e1, e2, _) -> Cmpop (op, re e1, re e2, r)
  | Not (e, _) -> Not (re e, r)
  | IfElse (cond, ift, iff, _) -> IfElse (re cond, re ift, re iff, r)
  | Let ((n, t), ((Fun _) as e), b, _) ->
      let unique_name = gen_unique_name n in
      let varenv' = (n, unique_name)::varenv in
      (* Must be letrec for functions *)
      Let ((string_of_unique_name unique_name, t),
           rename varenv' e, rename varenv' b, r)
  | Let ((n, t), e, b, _) ->
      let unique_name = gen_unique_name n in
      let varenv' = (n, unique_name)::varenv in
      Let ((string_of_unique_name unique_name, t), re e, rename varenv' b, r)
  | Fun (formals, ty, body, _) ->
      let (formalNames, formalTypes) = List.split formals in
      let uniqueFormalNames = List.map gen_unique_name formalNames in
      let addToVarenv = List.combine formalNames uniqueFormalNames in
      let uniqueFormalNamesStrings =
        List.map string_of_unique_name uniqueFormalNames
      in
      Fun (List.combine uniqueFormalNamesStrings formalTypes, ty,
           rename (addToVarenv @ varenv) body, r)
  | App (f, args, _) -> App (re f, List.map re args, r)
