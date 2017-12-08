open Common
module A = L02ast
module LOW = L05mir

type operand = [
  | `Imm of int
  | `Var of symbol
  | `Data of string
]
and symbol = string

type mem = [
  | operand
  | `Indirect of operand
]

type inst =
  | Label of symbol
  (*        src,   dst   *)
  | Move of tree * tree
  (*        funname, result variable, args *)
  | Call of symbol * symbol * tree list
  (*         num_stack_variables *)
  | Enter of int
  | Ret of tree
  | Jump of symbol
  (*         op,       L,     R,     location *)
  | Cjump of A.cmpop * tree * tree * symbol


and tree = [
  | mem
  (*         op,   e  *)
  | `Unop of unop * mem
  (*         op,     L,     R    *)
  | `Binop of binop * mem * mem
  | `Empty
]

and binop = LOW.binop
and unop = LOW.unop

let rec string_of_inst = function
  | Label s -> s ^ ":"
  | Move (src, `Var dst) -> "  " ^ dst ^ " <- " ^ string_of_tree src
  | Move (src, dst) -> "  (" ^ string_of_tree dst ^ ") <- " ^ string_of_tree src
  | Call (f, res, args) ->
      "  " ^ string_of_tree (`Var res) ^ " <- " ^ f ^ "(" ^
      S.map_concat ", " string_of_tree args ^ ")"
  | Enter i -> "enter with " ^ string_of_int i ^ " vars"
  | Ret v -> "  ret " ^ string_of_tree v
  | Jump l -> "  jump " ^ l
  | Cjump (o, l, r, loc) ->
      "if (" ^ string_of_tree l ^ " " ^ string_of_binop (LOW.Cmp o) ^ " "
      ^ string_of_tree r ^ ")" ^ string_of_inst (Jump loc)
and string_of_tree : tree -> string = function
  | `Imm i -> "$" ^ string_of_int i
  | `Var s -> s
  | `Data s -> "@" ^ s

  | `Indirect t -> "*(" ^ string_of_tree (t :> tree) ^ ")"

  | `Unop (o, e) -> string_of_unop o ^ "(" ^ string_of_tree (e :> tree) ^ ")"
  | `Binop (o, l, r) ->
      string_of_tree (l :> tree) ^ " "  ^ string_of_binop o ^ " "
      ^ string_of_tree (r :> tree)
  | `Empty -> "Nop"
and string_of_unop = LOW.string_of_unop
and string_of_binop = LOW.string_of_binop

type 'a fundecl = symbol * A.vardecl list * Types.ty
type funrec = { fundecl : Types.renamed fundecl; impl : inst list; }
type funrep = Fun of funrec

let string_of_funrep = function
  | Fun ({ fundecl = (name, formals, ty); impl = insts; }) ->
      "function " ^ name ^ "(" ^ S.map_concat ", " A.string_of_vardecl formals
      ^ "):\n" ^ S.map_concat "\n" string_of_inst insts ^ "\n"

let string_of_program = S.map_concat "\n" string_of_funrep

let variableCounter = ref []
let genVariable s =
  match L.assoc_opt s !variableCounter with
  | Some c ->
      let l = s ^ string_of_int c in
      variableCounter := (s, c+1):: !variableCounter;
      l
  | None ->
      variableCounter := (s, 1):: !variableCounter;
      s
let nextVariable () = genVariable "v"

exception NotImplemented of string

let lower : LOW.funrep list -> funrep list = fun funs ->
  let rec lower_tree = function
    | LOW.Empty ->
        let res = nextVariable () in
        (`Var res, [ Move (`Empty, `Var res); ])
    | LOW.Imm i ->
        let res = nextVariable () in
        (`Var res, [ Move (`Imm i, `Var res); ])
    | LOW.String s ->
        let res = nextVariable () in
        (`Var res, [ Move (`Data s, `Var res); ])
    | LOW.Var s -> (`Var s, [])
    | LOW.Unop (LOW.Not, e) ->
        let res = nextVariable () in
        let (de, eins) = lower_tree e in
        (`Var res, eins @ [ Move (`Unop (LOW.Not, de), `Var res); ])
    | LOW.Binop (o, l, r) ->
        let res = nextVariable () in
        let (dl, lins) = lower_tree l in
        let (dr, rins) = lower_tree r in
        (`Var res, lins @ rins @ [ Move (`Binop (o, dl, dr), `Var res); ])
    | LOW.Mem _ -> raise @@ NotImplemented "lowering Mem"
  in
  let lower_inst = function
    | LOW.Label s -> [ Label s ]
    | LOW.Move (src, dst) ->
        let (src', sins) = lower_tree src in
        let (dst', dins) = lower_tree dst in
        sins @ dins @ [ Move ((src' :> tree), (dst' :> tree)); ]
    | LOW.Call (f, dst, args) ->
        let (args', (prelude : inst list list)) =
          List.split @@ List.map lower_tree args
        in (List.concat prelude) @ [ Call (f, dst, (args' :> tree list)) ]
    | LOW.Ret t ->
        let (td, tins) = lower_tree t in
        tins @ [ Ret (td :> tree) ]
    | LOW.Jump l -> [ Jump l ]
    | LOW.Cjump (o, l, r, loc) ->
        let (dl, lins) = lower_tree l in
        let (dr, rins) = lower_tree r in
        lins @ rins @ [ Cjump (o, (dl :> tree), (dr :> tree), loc) ]
  in
  let lo (LOW.Fun ({ LOW.fundecl = fundecl; LOW.impl = insts; })) =
    let insts' = L.map_concat lower_inst insts in
    Fun ({ fundecl = fundecl; impl = insts' })
  in List.map lo funs
