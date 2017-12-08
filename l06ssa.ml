open Common
module A = L02ast

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
  | Move (src, Var dst) -> "  " ^ dst ^ " <- " ^ string_of_tree src
  | Move (src, dst) -> "  (" ^ string_of_tree dst ^ ") <- " ^ string_of_tree src
  | Call (f, res, args) ->
      "  " ^ string_of_tree (Var res) ^ " <- " ^ f ^ "(" ^
      S.map_concat ", " string_of_tree args ^ ")"
  | Enter i -> "enter with " ^ string_of_int i ^ " vars"
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
type funrec = { fundecl : Types.renamed fundecl; impl : inst list; stackSpace : int}
type funrep = Fun of funrec

let string_of_funrep = function
  | Fun ({ fundecl = (name, formals, ty); impl = insts; stackSpace = ss }) ->
      "function " ^ name ^ "(" ^ S.map_concat ", " A.string_of_vardecl formals
      ^ ") {" ^ string_of_int ss ^ "}:\n  "
      ^ S.map_concat "\n" string_of_inst insts ^ "\n"

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

let lower : funrep list -> funrep list =
  | Fun ({ fundecl = (name, formals, ty); impl = insts; stackSpace = ss }) ->
