module A = L02ast

type inst =
  | Label of symbol
  | Move of tree * tree
  (*        funname, result variable, args *)
  | Call of symbol * symbol * tree list
  | Ret of tree
  | Jump of symbol
  (*         op,     L,     R,     location *)
  | Cjump of cmpop * tree * tree * symbol

and tree =
  | Imm of int
  | Offset of symbol
  | String of string
  | Mem of tree
  | Var of symbol
  (*         op,   e  *)
  | Unop of unop * tree
  (*         op,     L,     R    *)
  | Binop of binop * tree * tree
  | Empty

and unop = Not | Neg
and binop = Plus | Minus | Times | Divide
and cmpop = Eq | Neq | Lt | Lte | Gt | Gte
and symbol = string

let rec string_of_inst = function
  | Label s -> "L:" ^ s
  | Move (dst, src) -> "(" ^ string_of_tree dst ^ ") <- " ^ string_of_tree src
  | Call (f, res, args) ->
      res ^ " <- " ^ f ^ "(" ^ (String.concat ", " @@ List.map string_of_tree args)
  | Ret v -> "ret " ^ string_of_tree v
  | Jump l -> "jump " ^ l
  | Cjump (o, l, r, loc) ->
      "if (" ^ string_of_tree l ^ string_of_cmpop o
      ^ string_of_tree r ^ ") " ^ string_of_inst (Jump loc)
and string_of_tree = function
  | Empty -> "Nop"
  | Imm i -> "$" ^ string_of_int i
  | Offset s -> s
  | String s -> "@" ^ s
  | Mem t -> "*(" ^ string_of_tree t ^ ")"
  | Var s -> "V:" ^ s
  | Unop (o, e) -> string_of_unop o ^ "(" ^ string_of_tree e ^ ")"
  | Binop (o, l, r) -> string_of_tree l ^ string_of_binop o ^ string_of_tree r
and string_of_unop = function | Not -> "!" | Neg -> "-"
and string_of_cmpop o =
  " " ^ (function | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">=") o ^ " "
and string_of_binop o =
  " " ^ (function | Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/") o ^ " "

(* -- Representation for a function
      a FUNDECL and a list of instructions

datatype funrep = FUN of { fundecl: A.decl, impl: inst list }
*)

type 'a fundecl = symbol * (symbol Types.env) option * symbol option * 'a A.exp

type funrec = { fundecl : Types.renamed fundecl; impl : inst list }
type funrep = Fun of funrec

exception Unimplemented

let rec lower : Types.renamed A.exp -> tree * inst list * funrep list = function
  | A.BoolLit (true, _) -> (Imm 1, [], [])
  | A.BoolLit (false, _) -> (Imm 0, [], [])
  | A.IntLit (i, _) -> (Imm i, [], [])
  | A.UnitLit _ -> (Empty, [], [])
  | A.AtomLit (a, _) -> (String a, [], [])
  | A.Var (n, _) -> (Var n, [], [])

  | A.Plus (l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (Plus, expL, expR), insL @ insR, funsL @ funsR)
  | A.Minus (l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (Minus, expL, expR), insL @ insR, funsL @ funsR)
  | A.Times (l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (Times, expL, expR), insL @ insR, funsL @ funsR)
  | A.Divide (l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (Divide, expL, expR), insL @ insR, funsL @ funsR)

  | A.Not (e, _) ->
      let (loweredE, instsE, funsE) = lower e in
      (Unop (Not, loweredE), instsE, funsE)

      (*
  | A.Lt (l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Cmp
      *)

  | A.Let ((n, _), e, body, _) ->
      let (expE, insE, funsE) = lower e in
      let (expBody, insBody, funsBody) = lower body in
      (expBody, insE @ [ Move (Var n, expE) ] @ insBody, funsE @ funsBody)

      (*
  | A.Fun (formals, ty, body, _) ->
      (Empty, 
      *)
