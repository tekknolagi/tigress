module A = L02ast

type inst =
  | Label of symbol
  (*        dst,   src    *)
  | Move of tree * tree
  (*        funname, result variable, args *)
  | Call of symbol * symbol * tree list
  | Ret of tree
  | Jump of symbol
  (*         op,     L,     R,     location *)
  | Cjump of A.cmpop * tree * tree * symbol

and tree =
  | Imm of int
  | Offset of symbol
  | String of string
  | Mem of tree
  | Var of symbol
  (*         op,   e  *)
  (*
  | Unop of unop * tree
  *)
  (*         op,        L,     R    *)
  | Binop of A.mathop * tree * tree
  | Empty

and unop = Not | Neg
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
  (*
  | Unop (o, e) -> string_of_unop o ^ "(" ^ string_of_tree e ^ ")"
  *)
  | Binop (o, l, r) -> string_of_tree l ^ string_of_binop o ^ string_of_tree r
and string_of_unop = function | Not -> "!" | Neg -> "-"
and string_of_cmpop o =
  let open A in
  " " ^ (function | Equals -> "==" | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">=") o ^ " "
and string_of_binop o =
  let open A in
  " " ^ (function | Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/") o ^ " "

(* -- Representation for a function
      a FUNDECL and a list of instructions

datatype funrep = FUN of { fundecl: A.decl, impl: inst list }
*)

type 'a fundecl = symbol * (symbol Types.env) option * symbol option * 'a A.exp

type funrec = { fundecl : Types.renamed fundecl; impl : inst list }
type funrep = Fun of funrec

exception Unimplemented

(* TODO: make this so that it is a per-name counter *)
let labelCounter = ref 0
let genLabel s =
  let l = "__" ^ s ^ (string_of_int !labelCounter) in
  labelCounter := !labelCounter + 1;
  l

let rec lower : Types.renamed A.exp -> tree * inst list * funrep list = function
  | A.BoolLit (true, _) -> (Imm 1, [], [])
  | A.BoolLit (false, _) -> (Imm 0, [], [])
  | A.IntLit (i, _) -> (Imm i, [], [])
  | A.UnitLit _ -> (Empty, [], [])
  | A.AtomLit (a, _) -> (String a, [], [])
  | A.Var (n, _) -> (Var n, [], [])

  | A.Mathop (op, l, r, _) ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (op, expL, expR), insL @ insR, funsL @ funsR)

  (* TODO: add special cases for and/or *)
  | A.Cmpop (op, l, r, ann) as c ->
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      let cmpOutputVar = genLabel "outputVar" in
      let trueBranch = genLabel "trueBranch" in
      let endOfBlock = genLabel "endOfBlock" in


      ( Var cmpOutputVar,
        insL @
        insR @
        [
          Cjump (op, expL, expR, trueBranch);
          Move (Var cmpOutputVar, Imm 0);
          Jump endOfBlock;
          Label trueBranch;
          Move (Var cmpOutputVar, Imm 1);
          Label endOfBlock;
        ],
        funsL @ funsR )



  | A.IfElse (cond, ift, iff, _) ->
      let (expCond, insCond, funsCond) = lower cond in
      let (expT, insT, funsT) = lower ift in
      let (expF, insF, funsF) = lower iff in

      let ifOutputVar = genLabel "outputVar" in
      let falseBranch = genLabel "falseBranch" in
      let endOfBlock = genLabel "endOfBlock" in

      ( Var ifOutputVar,
        insCond @
        [ Cjump (Equals, expCond, Imm 0, falseBranch); ] @
        insT @
        [
          Move (Var ifOutputVar, expT);
          Jump endOfBlock;
          Label falseBranch;
        ] @
        insF @
        [
          Move (Var ifOutputVar, expF);
          Label endOfBlock;
        ],
        funsCond @ funsT @ funsF
      )


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
