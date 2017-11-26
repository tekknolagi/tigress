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


exception TypeError of string

let tyOf : 'a exp -> 'a = function
  | BoolLit (_, a) | IntLit (_, a) | AtomLit (_, a) | Var (_, a) -> a
  | UnitLit a -> a
  | Mathop (_, _, _, a) -> a | Cmpop (_, _, _, a) -> a | Not (_, a) -> a
  | IfElse (_, _, _, a) | Let (_, _, _, a) -> a
  | Fun (_, _, _, a) -> a | App (_, _, a) -> a

let tyMismatch : string -> ty -> ty -> 'a = fun what exp act ->
  raise @@ TypeError ("Type mismatch in " ^ what ^ ": expected "
  ^ string_of_ty exp ^ ", but got " ^ string_of_ty act ^ " instead")

let rec typecheck varenv (exp : unit exp) : ty exp =
  let ty = typecheck varenv in
  match exp with
  | BoolLit (b, _) -> BoolLit (b, BoolTy)
  | IntLit (i, _) -> IntLit (i, IntTy)
  | UnitLit _ -> UnitLit UnitTy
  | AtomLit (at, _) -> AtomLit (at, AtomTy)
  | Var (n, _) ->
      let tyN =
        try List.assoc n varenv
        with Not_found ->
          raise @@ TypeError ("Could not find " ^ n ^ " in tyenv")
      in Var (n, tyN)
  | Mathop (op, e1, e2, _) ->
      let opStr = List.assoc op [Plus,"+"; Minus,"-"; Times,"*"; Divide,"/"] in
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Mathop (op, t1, t2, IntTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch opStr IntTy q)
  | Cmpop (Equals, e1, e2, _) ->
      let (t1, t2) = (ty e1, ty e2) in
      if tyOf t1 <> tyOf t2 then tyMismatch "=" (tyOf t1) (tyOf t2);
      Cmpop (Equals, t1, t2, BoolTy)
  | Cmpop (op, e1, e2, _) ->
      let opStr = List.assoc op [Lt,"<"; Lte,"<="; Gt,">"; Gte,">="] in
      let (t1, t2) = (ty e1, ty e2) in
      (match (tyOf t1, tyOf t2) with
      | (IntTy, IntTy) -> Cmpop (op, t1, t2, BoolTy)
      | (IntTy, q) | (q, IntTy) | (q, _) -> tyMismatch opStr IntTy q)
  | Not (e, _) ->
      let te = ty e in
      if tyOf te <> BoolTy then tyMismatch "not" BoolTy (tyOf te);
      Not (te, BoolTy)
  | IfElse (cond, ift, iff, _) ->
      let tcond = ty cond in
      if tyOf tcond <> BoolTy then tyMismatch "if" BoolTy (tyOf tcond);
      let (tift, tiff) = (ty ift, ty iff) in
      if tyOf tift <> tyOf tiff then tyMismatch "if" (tyOf tift) (tyOf tiff);
      IfElse (tcond, tift, tiff, tyOf tift)
  (* TODO: cleanup *)
  | Let ((n, t), (Fun (formals, fret, fbody, _) as e), b, _) ->
      let tyFormals = List.map snd formals in
      let tyN = FunTy (tyFormals, fret) in
      let varenv' = (n,tyN)::varenv in
      let te = typecheck varenv' e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let ((n, t), te, tb, tyOf tb)
  | Let ((n, t), e, b, _) ->
      let te = ty e in
      if t <> tyOf te then tyMismatch "let" t (tyOf te);
      let tb = typecheck ((n, tyOf te)::varenv) b in
      Let ((n, t), te, tb, tyOf tb)
  | Fun (formals, ty, body, _) ->
      let tyFormals = List.map snd formals in
      let varenv' = formals @ varenv in
      let tbody = typecheck varenv' body in
      if tyOf tbody <> ty then tyMismatch "fun" ty (tyOf tbody);
      Fun (formals, ty, tbody, FunTy (tyFormals, ty))
  | App (f, actuals, _) ->
      let typedActuals = List.map ty actuals in
      let typesOfActuals = List.map tyOf typedActuals in
      let typed_f = ty f in
      (match tyOf typed_f with
      | FunTy (ts_formals, retTy) ->
          if typesOfActuals <> ts_formals
          then tyMismatch "apply" (tyOf typed_f) (FunTy (typesOfActuals, retTy))
          else App (typed_f, typedActuals, retTy)
      | _ -> raise @@ TypeError "non-function applied to arguments")



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

exception BugInRenaming of string
type unique_name = name * int
let string_of_unique_name (n, c) = n ^ string_of_int c
let rec rename (varenv : unique_name env) (exp : ty exp) : renamed exp =
  let re = rename varenv in
  let is_being_shadowed = List.mem_assoc in
  let get_last_counter n h = snd @@ List.assoc n h in
  match exp with
  | BoolLit (b, _) -> BoolLit (b, `Renamed)
  | IntLit (i, _) -> IntLit (i, `Renamed)
  | UnitLit _ -> UnitLit `Renamed
  | AtomLit (a, _) -> AtomLit (a, `Renamed)
  | Var (n, _) ->
      let foundVariable = List.assoc n varenv in
      Var (string_of_unique_name foundVariable, `Renamed)
  | Mathop (op, e1, e2, _) -> Mathop (op, re e1, re e2, `Renamed)
  | Cmpop (op, e1, e2, _) -> Cmpop (op, re e1, re e2, `Renamed)
  | Not (e, _) -> Not (re e, `Renamed)
  | IfElse (cond, ift, iff, _) -> IfElse (re cond, re ift, re iff, `Renamed)
  | Let ((n, t), e, b, _) ->
      let new_var =
        if is_being_shadowed n varenv
        then (n, 1 + get_last_counter n varenv)
        else (n, 0)
      in
      let varenv' = (n, new_var)::varenv in
      Let ((string_of_unique_name new_var, t), re e, rename varenv' b, `Renamed)
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
           `Renamed)
  | App (f, args, _) -> App (re f, List.map re args, `Renamed)
  *)
