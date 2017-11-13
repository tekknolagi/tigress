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
  (*         op,     L,     R    *)
  | Binop of binop * tree * tree
  | Empty

and binop = Plus | Minus | Times | Div
and cmpop = Eq | Neq | Lt | Gt | Gte
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
  | Binop (o, l, r) -> string_of_tree l ^ string_of_binop o ^ string_of_tree r
and string_of_cmpop o =
  " " ^ (function | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Gte -> ">=") o ^ " "
and string_of_binop o =
  " " ^ (function | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/") o ^ " "

type funrep = Fun of { fundecl : A.vardecl; impl : inst list }



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
      let newTree = Binop (Plus, expL, expR) in
      (newTree, insL @ insR, funsL @ funsR)

  | A.Let ((n, _), e, body, _) ->
      let (expE, insE, funsE) = lower e in
      let (expBody, insBody, funsBody) = lower body in

      let assign = Move (Var n, expE) in
      (expBody, insE @ [ assign ] @ insBody, funsE @ funsBody)
