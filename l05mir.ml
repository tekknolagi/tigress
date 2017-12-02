open Common
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
  | Binop of binop * tree * tree
  | Empty

and binop = Math of A.mathop | Cmp of A.cmpop
and unop = Not | Neg
and symbol = string

let rec string_of_inst = function
  | Label s -> "L:" ^ s
  | Move (dst, src) -> "(" ^ string_of_tree dst ^ ") <- " ^ string_of_tree src
  | Call (f, res, args) ->
      string_of_tree (Var res) ^ " <- " ^ f ^ "(" ^
      (String.concat ", " @@ List.map string_of_tree args) ^ ")"
  | Ret v -> "ret " ^ string_of_tree v
  | Jump l -> "jump " ^ l
  | Cjump (o, l, r, loc) ->
      "if (" ^ string_of_tree l ^ " " ^ string_of_binop (Cmp o) ^ " "
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
  | Binop (o, l, r) ->
      string_of_tree l ^ " "  ^ string_of_binop o ^ " " ^ string_of_tree r
and string_of_unop = function | Not -> "!" | Neg -> "-"
and string_of_binop = let open A in function
  | Math m -> List.assoc m [Plus,"+"; Minus,"-"; Times,"*"; Divide,"/"]
  | Cmp  c -> List.assoc c [Equals,"=="; Lt,"<"; Lte,"<="; Gt,">"; Gte,">="]

type 'a fundecl = symbol * A.vardecl list * Types.ty
type funrec = { fundecl : Types.renamed fundecl; impl : inst list }
type funrep = Fun of funrec

let string_of_funrep = function
  | Fun ({ fundecl = (name, formals, ty); impl = insts }) ->
      "function " ^ name ^ "(" ^ (
        String.concat ", " @@ List.map A.string_of_vardecl formals
      ) ^ "):\n  " ^
      String.concat "\n  " @@ List.map string_of_inst insts

exception Unimplemented

let labelCounter = ref []
let genLabel s =
  match L.assoc_opt s !labelCounter with
  | Some c ->
      let l = "__" ^ s ^ (string_of_int c) in
      labelCounter := (s, c+1):: !labelCounter;
      l
  | None ->
      labelCounter := (s, 1):: !labelCounter;
      s

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
      (Binop (Math op, expL, expR), insL @ insR, funsL @ funsR)
  | A.Cmpop (op, l, r, _) ->
      (* TODO: add special cases for and/or *)
      let (expL, insL, funsL) = lower l in
      let (expR, insR, funsR) = lower r in
      (Binop (Cmp op, expL, expR), insL @ insR, funsL @ funsR)

  | A.IfElse (cond, ift, iff, _) ->
      let (expCond, insCond, funsCond) = lower cond in
      let (expT, insT, funsT) = lower ift in
      let (expF, insF, funsF) = lower iff in

      let ifOutputVar = genLabel "outputVar" in
      let falseBranch = genLabel "falseBranch" in
      let endOfBlock = genLabel "endOfBlock" in

      ( Var ifOutputVar,
        insCond @
        [ Cjump (A.Equals, expCond, Imm 0, falseBranch); ] @
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
  | A.Not (e, _) ->
      let (loweredE, instsE, funsE) = lower e in
      (Unop (Not, loweredE), instsE, funsE)
      *)

  | A.Fun (formals, ty, body, ann) ->
      lower body

  | A.App ((Var (fn, _)) as f, actuals, ann) ->
      let (expF, insF, funsF) = lower f in
      let loweredActuals = List.map lower actuals in
      let expActuals = List.map (fun (e, _, _) -> e) loweredActuals in
      let insActuals =
        List.concat @@ List.map (fun (_, i, _) -> i) loweredActuals
      in
      let funActuals =
        List.concat @@ List.map (fun (_, _, f) -> f) loweredActuals
      in
      let resultVariable = genLabel "return" in
      ( Var resultVariable,
        insActuals @ [
          Call (fn, resultVariable, expActuals);
        ],
        funActuals )

  (* | Fun of (vardecl list * ty * 'a exp * 'a) *)
  | A.Let ((n, _), (A.Fun (formals, ty, funBody, ann) as f), body, _) ->
      let (_expBody, insBody, funsBody) = lower body in
      let (_expFunBody, insFunBody, funsFunBody) = lower f in
      (Empty, [], funsBody @ funsFunBody @ [
        Fun ({
          fundecl = (n, formals, ty);
          impl = insFunBody;
        })
      ])

      (*
      let (expE, insE, funsE) = lower e in
      let (expBody, insBody, funsBody) = lower body in
      (expBody, insE @ [ Move (Var n, expE) ] @ insBody, funsE @ funsBody)
      *)

  | A.Let ((n, _), e, body, _) ->
      let (expE, insE, funsE) = lower e in
      let (expBody, insBody, funsBody) = lower body in
      (expBody, insE @ [ Move (Var n, expE) ] @ insBody, funsE @ funsBody)
