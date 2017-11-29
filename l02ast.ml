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
and cmpop = Lt | Lte | Gt | Gte | Equals

and vardecl = name * ty
and name = string

and value =
  | BoolVal of bool
  | IntVal of int
  | UnitVal
  | AtomVal of string
  | ClosureVal of (name list * ty exp * value ref env)
  | Unspecified
let string_of_value = function
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | UnitVal -> "()"
  | AtomVal a -> a
  | ClosureVal _ -> "(closure)"
  | Unspecified -> "UNSPECIFIED"

let rec string_of_aexp f exp =
  let toS = string_of_aexp f in
  let ann a = let s = f a in if s="" then "" else " : " ^ s in
  let fmt2 tag l r a = tag ^ " (" ^ toS l ^ ", " ^ toS r ^ ")" ^ ann a in
  let string_of_vardecl (n, t) = n ^ " : " ^ string_of_ty t in
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


    (*
let gensym =
  let module Counter = struct
    type t = { mutable counter : int }
    let make () = { counter = 0 }
    let inc ctr =
      let c = ctr.counter in
      ctr.counter <- ctr.counter + 1;
      c
    let next name ctr = (name, inc ctr)
  end in
  let symcounter = Counter.make () in
  (fun () -> Counter.next "__var" symcounter)

[@@@ warning "-8"] (* Ignore non-exhaustive *)
exception BugInRenaming of string
type unique_name = name * int
let string_of_unique_name (n, c) = n ^ string_of_int c
let rec rename (varenv : unique_name env) (exp : ty exp) : renamed exp =
  let re = rename varenv in
  let r = `Renamed in
  let is_being_shadowed = List.mem_assoc in
  let get_last_counter n h = snd @@ List.assoc n h in
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
  | Let ((n, t), e, b, _) ->
      let new_var =
        if is_being_shadowed n varenv
        then (n, 1 + get_last_counter n varenv)
        else (n, 0)
      in
      let varenv' = (n, new_var)::varenv in
      Let ((string_of_unique_name new_var, t), re e, rename varenv' b, r)
      (*
  | Fun (formals, ty, body, _) ->
      let (formalNames, formalTypes) = List.split formals in
      let newFormalNames =
        List.map string_of_unique_name @@
        List.map (fun _ -> gensym ()) formalNames in
      let addToVarenv = List.combine formalNames newFormalNames in
      Fun (List.combine newFormalNames formalTypes,
           ty,
           rename (addToVarenv @ varenv) body,
           r)
  | App (f, args, _) -> App (re f, List.map re args, r)
  *)
      *)
