(*
http://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
*)

open Common
module AST = L02ast
module LOW = L05mir

type symbol = string
type t = Fundecl of string * string list * string * string

type register = Hardware of string * int | Virtual of symbol
let string_of_reg = function
  | Hardware (nm, i) -> "%" ^ nm
  | Virtual s -> "%" ^ s

let rax = Hardware("rax", 0)
let rcx = Hardware("rcx", 1)
let rdx = Hardware("rdx", 2)
let rbx = Hardware("rbx", 3)
let rsi = Hardware("rsi", 4)
let rdi = Hardware("rdi", 5)
let rsp = Hardware("rsp", 6)
let rbp = Hardware("rbp", 7)
let r8  = Hardware("r8", 8)
let r9  = Hardware("r9", 9)
let r10 = Hardware("r10", 10)
let r11 = Hardware("r11", 11)
let r12 = Hardware("r12", 12)
let r13 = Hardware("r13", 13)
let r14 = Hardware("r14", 14)
let r15 = Hardware("r15", 15)

type operand = Imm of int
             | Data of string
             | Var of symbol                          (* Unallocated variable *)
             | Direct of register                     (* %r12 *)
             | Indirect of register                   (* (%rax) *)
             | Offset of int * register               (* -16(%rax) *)
             | Scaled of register * register * int    (* (%rax, %rcx, 8) *)
let string_of_operand =
  let paren s = "(" ^ s ^ ")" in
  let sep = String.concat in
  function
  | Imm i -> "$" ^ string_of_int i
  | Data s -> "\"" ^ s ^ "\""
  | Var s -> s (* TODO: fix? *)
  | Direct r -> string_of_reg r
  | Indirect r -> paren @@ string_of_reg r
  | Offset (off, r) -> string_of_int off ^ paren @@ string_of_reg r
  | Scaled (b, off, sc) ->
      paren (sep "," [string_of_reg b; string_of_reg off; string_of_int sc])


(* OP src, dst *)
type x64ins = Move  of operand * operand
            | Push  of operand
            | Pop   of operand
            | Inc   of operand
            | Dec   of operand
            | Neg   of operand
            | Not   of operand
            | Lea   of operand * operand
            | Add   of operand * operand
            | Sub   of operand * operand
            | Imul  of operand * operand
            | Idiv  of operand * operand
            | Xor   of operand * operand
            | Or    of operand * operand
            | And   of operand * operand
            | Shl   of operand * operand
            | Shr   of operand * operand
            | Cmp   of operand * operand
            | Test  of operand * operand
            | Jmp   of symbol
            | Je    of symbol
            | Jne   of symbol
            | Jg    of symbol
            | Jge   of symbol
            | Jl    of symbol
            | Jle   of symbol
            | Label of symbol
            | Call  of symbol
            | Leave
            | Ret
            | Comment of string (* x64 Comment for debugging *)
let string_of_ins =
  let insString1 s op1     = "  " ^ s ^ "     " ^ (string_of_operand op1) in
  let insString2 s op1 op2 =
    "  " ^ s ^ "     " ^ (string_of_operand op1) ^ ", " ^ (string_of_operand op2)
  in
  function
  | Move (s, d) -> insString2 "movq " s d 
  | Push s      -> insString1 "pushq" s
  | Pop s       -> insString1 "popq " s
  | Inc s       -> insString1 "incq " s
  | Dec s       -> insString1 "decq " s
  | Neg s       -> insString1 "negq " s
  | Not s       -> insString1 "notq " s
  | Lea (s, d)  -> insString2 "leaq " s d
  | Add (s, d)  -> insString2 "addq " s d
  | Sub (s, d)  -> insString2 "subq " s d
  | Imul (s, d) -> insString2 "imull" s d
  | Idiv (s, d) -> insString2 "idivl" s d
  | Xor (s, d)  -> insString2 "xorq " s d
  | Or (s, d)   -> insString2 "orq  " s d
  | And (s, d)  -> insString2 "andq " s d
  | Shl (s, d)  -> insString2 "shlq " s d
  | Shr (s, d)  -> insString2 "shrq " s d
  | Cmp (s, d)  -> insString2 "cmpq " s d
  | Test (s, d) -> insString2 "testq" s d
  | Jmp l       -> "  jmp " ^ l
  | Je l        -> "  je  " ^ l
  | Jne l       -> "  jne " ^ l
  | Jg l        -> "  jg  " ^ l
  | Jge l       -> "  jge " ^ l
  | Jl l        -> "  jl  " ^ l
  | Jle l       -> "  jle " ^ l
  | Label l     -> l ^ ":"
  | Call f      -> "  call  " ^ f
  | Leave       -> "  leaveq"
  | Ret         -> "  ret"
  | Comment s   -> "  /* " ^ s ^ " */"


type x64funrec = { mirfun : LOW.funrep; impl : x64ins list }
type x64fun = Fun of x64funrec

exception NotImplemented
exception InternalError of string


let genVariable = LOW.genVariable


let rec generateIns ss = function
  | LOW.Label l -> [ Label l ]
  | LOW.Move (_, LOW.Empty) -> []
  | LOW.Move (t1, t2) ->
      let (t1op, t1ins) = generateOperand t1 in
      let (t2op, t2ins) = generateOperand t2 in
      t2ins @ t1ins @ [ Move (t2op, t1op) ]
  | LOW.Call ("Write", "Q", args) ->
      (* ssize_t write(int fd, const void *buf, size_t count); *)
      raise NotImplemented
  | LOW.Call (f, ret, args) -> raise NotImplemented
  | LOW.Enter 0 -> [
    Comment "Enter";
    Push (Direct rbp);
    Move (Direct rsp, Direct rbp);
  ]
  | LOW.Enter i -> [
    Comment "Enter";
    Push (Direct rbp);
    Move (Direct rsp, Direct rbp);
    Sub (Imm i, Direct rsp);
  ]
  | LOW.Ret r ->
      let (rop, rins) = generateOperand r in
      let stackManip = if ss=0 then [] else [ Add (Imm ss, Direct rsp) ] in
      rins @ [ Move (rop, Direct rax); ] @ stackManip @ [ Leave; Ret ]
  | LOW.Jump l -> [ Jmp l ]
  | LOW.Cjump (cmpop, t1, t2, l) -> raise NotImplemented

and generateOperand =
  let rec gen_ins mathop d1 d2 =
    match d2 with
    | Imm i -> 
        let res = Var (genVariable "v") in
        let (res', ins) = gen_ins mathop d1 res in
        (res', (Move (d2, res))::ins)
    | d2 ->
      (d2, [ (match mathop with
       | AST.Plus ->   Add (d1, d2)
       | AST.Minus ->  Sub (d1, d2)
       | AST.Times ->  Imul (d1, d2)
       | AST.Divide -> Idiv (d1, d2)) ])
  in
  function
  | LOW.Imm i -> (Imm i, [])
  | LOW.Offset field -> raise NotImplemented
  | LOW.String s -> (Data s, [])
  | LOW.Mem t -> raise NotImplemented
  | LOW.Var s -> raise NotImplemented
  | LOW.Unop (op, t1) -> raise NotImplemented
  (*
  | LOW.Binop (LOW.Math mathop, LOW.Imm i1, LOW.Imm i2) ->
      let res = Var (genVariable "v") in
      (res, [ Move (Imm i1, res) ] @ gen_ins mathop (Imm i2) res)
      *)
  | LOW.Binop (LOW.Math mathop, t1, t2) ->
      let (d1, ins1) = generateOperand t1 in
      let (d2, ins2) = generateOperand t2 in
      let (d, ins) = gen_ins mathop d1 d2 in
      (d, ins1 @ ins2 @ ins)
  | LOW.Binop (LOW.Cmp cmpop, t1, t2) ->
      raise NotImplemented
  | LOW.Empty -> raise NotImplemented

and generateRegister = function
  | (LOW.Imm i, reg) ->
      raise NotImplemented
      (*
      [
    Move (Imm i, Direct reg)
  ]
  *)
  | (LOW.Offset field, reg) -> raise @@ InternalError "No register for offset"
  | (LOW.String s, reg) -> [
    Move (Data s, Direct reg);
  ]
  | (LOW.Mem t, reg) -> raise @@ InternalError "No register for mem"
  | (LOW.Var s, reg) -> raise NotImplemented
  | (LOW.Binop (LOW.Math binop, t1, t2), reg) ->
      let t1ops = generateRegister (t1, reg) in
      let t2ops = generateRegister (t2, reg) in
      t1ops @ [
        Move (Direct reg, Direct (Virtual "tmp"));
      ] @ t2ops
  | (LOW.Binop (binop, t1, t2), reg) -> raise NotImplemented
  | (LOW.Empty, reg) -> raise @@ InternalError "Can't generate empty exp"
  | (LOW.Unop (LOW.Not, r), reg) -> raise NotImplemented

and generateFun ((LOW.Fun ({ LOW.fundecl = fundecl; LOW.impl = ins; stackSpace = ss})) as mirfun) =
  let x64ins = L.map_concat (generateIns ss) ins in
  Fun ({ mirfun = mirfun; impl = x64ins })


let generateProgram = List.map generateFun

let string_of_fun (Fun { mirfun = (LOW.Fun ({ LOW.fundecl = fundecl; LOW.impl = ins }));
                         impl = x64ins }) =
  let assembly = S.map_concat "\n" string_of_ins x64ins in
  let (name, args, ty) = fundecl in
  ".globl " ^ name ^ "\n" ^
  ".type     " ^ name ^ ", @function\n" ^
  name ^ ":\n" ^
  assembly ^ "\n"

let prelude = ".globl _start
_start:
   call main
   movq %rax, %rdi
   call _exit

_exit:
   movq %rdi, %rax
   movq $60, %rax   /* exit syscall */
   syscall

"

let string_of_program funcs = prelude ^ S.map_concat "" string_of_fun funcs
