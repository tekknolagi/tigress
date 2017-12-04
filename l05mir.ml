open Common
module A = L02ast

type inst =
  | Label of symbol
  (*        dst,   src    *)
  | Move of tree * tree
  (*        funname, result variable, args *)
  | Call of symbol * symbol * tree list
  | Enter
  | Ret of tree
  | Jump of symbol
  (*         op,       L,     R,     location *)
  | Cjump of A.cmpop * tree * tree * symbol

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

and binop = Math of A.mathop | Cmp of A.cmpop
and unop = Not
and symbol = string

let rec string_of_inst = function
  | Label s -> s ^ ":"
  | Move (Var dst, src) -> "  " ^ dst ^ " <- " ^ string_of_tree src
  | Move (dst, src) -> "  (" ^ string_of_tree dst ^ ") <- " ^ string_of_tree src
  | Call (f, res, args) ->
      "  " ^ string_of_tree (Var res) ^ " <- " ^ f ^ "(" ^
      (String.concat ", " @@ List.map string_of_tree args) ^ ")"
  | Enter -> "enter"
  | Ret v -> "  ret " ^ string_of_tree v
  | Jump l -> "  jump " ^ l
  | Cjump (o, l, r, loc) ->
      "if (" ^ string_of_tree l ^ " " ^ string_of_binop (Cmp o) ^ " "
      ^ string_of_tree r ^ ")" ^ string_of_inst (Jump loc)
and string_of_tree = function
  | Empty -> "Nop"
  | Imm i -> "$" ^ string_of_int i
  | Offset s -> s
  | String s -> "@" ^ s
  | Mem t -> "*(" ^ string_of_tree t ^ ")"
  | Var s -> s
  | Unop (o, e) -> string_of_unop o ^ "(" ^ string_of_tree e ^ ")"
  | Binop (o, l, r) ->
      string_of_tree l ^ " "  ^ string_of_binop o ^ " " ^ string_of_tree r
and string_of_unop = function | Not -> "!"
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
      (String.concat "\n" @@ List.map string_of_inst insts) ^ "\n"

let labelCounter = ref []
let genLabel s =
  "." ^ match L.assoc_opt s !labelCounter with
  | Some c ->
      let l = s ^ string_of_int c in
      labelCounter := (s, c+1):: !labelCounter;
      l
  | None ->
      labelCounter := (s, 1):: !labelCounter;
      s

let variableCounter = ref []
let genVariable s =
  "__" ^ match L.assoc_opt s !variableCounter with
  | Some c ->
      let l = s ^ string_of_int c in
      variableCounter := (s, c+1):: !variableCounter;
      l
  | None ->
      variableCounter := (s, 1):: !variableCounter;
      s

exception BugInLowering of string

let rec lower : Types.renamed A.exp -> funrep list =
  let rec gen_fundecl name formals ty body =
    let (expBody, insBody, funsBody) = lo body in
    let insBody' = [ Enter ] @ insBody @ [ Ret expBody ] in
    let n = match name with
            | Some n -> n
            | None -> genLabel "lambda"
    in
    (n, funsBody @ [Fun ({ fundecl = (n, formals, ty); impl = insBody'; })])
  and lo : Types.renamed A.exp -> tree * inst list * funrep list = function
  | A.BoolLit (true, _) -> (Imm 1, [], [])
  | A.BoolLit (false, _) -> (Imm 0, [], [])
  | A.IntLit (i, _) -> (Imm i, [], [])
  | A.UnitLit _ -> (Empty, [], [])
  | A.AtomLit (a, _) -> (String a, [], [])
  | A.Var (n, _) -> (Var n, [], [])

  | A.Mathop (op, l, r, _) ->
      let (expL, insL, funsL) = lo l in
      let (expR, insR, funsR) = lo r in
      (Binop (Math op, expL, expR), insL @ insR, funsL @ funsR)
  | A.Cmpop (A.And, l, r, _) ->
      lo @@ A.IfElse (l, r, (A.BoolLit (false, `Renamed)), `Renamed)
  | A.Cmpop (A.Or, l, r, _) ->
      lo @@ A.IfElse (l, (A.BoolLit (true, `Renamed)), r, `Renamed)
  | A.Cmpop (op, l, r, _) ->
      let (expL, insL, funsL) = lo l in
      let (expR, insR, funsR) = lo r in
      (Binop (Cmp op, expL, expR), insL @ insR, funsL @ funsR)

  | A.IfElse (cond, ift, iff, _) ->
      let (expCond, insCond, funsCond) = lo cond in
      let (expT, insT, funsT) = lo ift in
      let (expF, insF, funsF) = lo iff in
      let ifOutputVar = genVariable "outputVar" in
      let falseBranch = genLabel "falseBranch" in
      let endOfBlock = genLabel "endOfBlock" in
      ( Var ifOutputVar,
        insCond @ [
          Cjump (A.Equals, expCond, Imm 0, falseBranch);
        ] @ insT @ [
          Move (Var ifOutputVar, expT);
          Jump endOfBlock;
          Label falseBranch;
        ] @ insF @ [
          Move (Var ifOutputVar, expF);
          Label endOfBlock;
        ],
        funsCond @ funsT @ funsF
      )

  | A.Not (e, _) ->
      let (loweredE, instsE, funsE) = lo e in
      (Unop (Not, loweredE), instsE, funsE)

  | A.Fun (formals, ty, body, _) ->
      let (n, funsBody) = gen_fundecl None formals ty body in
      (Var n, [], funsBody)

  | A.App (f, actuals, ann) ->
      let (expF, insF, funsF) = lo f in
      let fn = (match expF with
                | Var fn -> fn
                | _ -> raise @@ BugInLowering "Fun did not return Var")
      in
      let loweredActuals = List.map lo actuals in
      let expActuals = List.map (fun (e, _, _) -> e) loweredActuals in
      let insActuals =
        List.concat @@ List.map (fun (_, i, _) -> i) loweredActuals
      in
      let funActuals =
        List.concat @@ List.map (fun (_, _, f) -> f) loweredActuals
      in
      let resultVariable = genVariable "result" in
      ( Var resultVariable,
        insF @ insActuals @ [ Call (fn, resultVariable, expActuals); ],
        funsF @ funActuals )

  | A.Let ((n, _), A.Fun (formals, ty, funBody, _), body, _) ->
      let (_, funsFunBody) = gen_fundecl (Some n) formals ty funBody in
      let (expBody, insBody, funsBody) = lo body in
      (expBody, insBody, funsFunBody @ funsBody)

  | A.Let ((n, _), e, body, _) ->
      let (expE, insE, funsE) = lo e in
      let (expBody, insBody, funsBody) = lo body in
      (expBody, insE @ [ Move (Var n, expE) ] @ insBody, funsE @ funsBody)

  in fun exp ->
    let (e, i, f) = lo exp in
    f @ snd @@ gen_fundecl (Some "main") [] Types.(FunTy ([], UnitTy)) exp
