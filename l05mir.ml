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
      String.concat "\n  " @@ List.map string_of_inst insts

exception Unimplemented

let labelCounter = ref []
let genLabel s =
  "__" ^ match L.assoc_opt s !labelCounter with
  | Some c ->
      let l = s ^ string_of_int c in
      labelCounter := (s, c+1):: !labelCounter;
      l
  | None ->
      labelCounter := (s, 1):: !labelCounter;
      s

exception BugInLowering of string

let rec lower : Types.renamed A.exp -> tree * inst list * funrep list =
  let gen_fundecl name formals ty body =
    let (expBody, insBody, funsBody) = lower body in
    let insBody' = insBody @ [ Ret expBody ] in
    let n = match name with
            | Some n -> n
            | None -> genLabel "lambda"
    in
    (n, funsBody @ [Fun ({ fundecl = (n, formals, ty); impl = insBody'; })])
  in
  function
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

  | A.Not (e, _) ->
      let (loweredE, instsE, funsE) = lower e in
      (Unop (Not, loweredE), instsE, funsE)

  | A.Fun (formals, ty, body, _) ->
      let (n, funsBody) = gen_fundecl None formals ty body in
      ( Var n, [], funsBody)

  | A.App (f, actuals, ann) ->
      let (expF, insF, funsF) = lower f in
      let fn = (match expF with
                | Var fn -> fn
                | _ -> raise @@ BugInLowering "Fun did not return Var")
      in
      let loweredActuals = List.map lower actuals in
      let expActuals = List.map (fun (e, _, _) -> e) loweredActuals in
      let insActuals =
        List.concat @@ List.map (fun (_, i, _) -> i) loweredActuals
      in
      let funActuals =
        List.concat @@ List.map (fun (_, _, f) -> f) loweredActuals
      in
      let resultVariable = genLabel "result" in
      ( Var resultVariable,
        insF @ insActuals @ [
          Call (fn, resultVariable, expActuals);
        ],
        funsF @ funActuals )

  | A.Let ((n, _), A.Fun (formals, ty, funBody, _), body, _) ->
      let (_, funsFunBody) = gen_fundecl (Some n) formals ty funBody in
      let (expBody, insBody, funsBody) = lower body in
      (expBody, insBody, funsFunBody @ funsBody)

  | A.Let ((n, _), e, body, _) ->
      let (expE, insE, funsE) = lower e in
      let (expBody, insBody, funsBody) = lower body in
      (expBody, insE @ [ Move (Var n, expE) ] @ insBody, funsE @ funsBody)
