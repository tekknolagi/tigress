{
  open L01parser
  open Types
  exception Eof
}

rule token = parse
| [' ' '\t' '\n']        { token lexbuf }
| "true"          { TBool true }
| "false"         { TBool false }
| "if"            { KIf }
| "then"          { KThen }
| "else"          { KElse }
| "end"           { KEnd }
| "let"           { KLet }
| "in"            { KIn }

| "Bool"          { Ty BoolTy }
| "Int"           { Ty IntTy }
| "Atom"          { Ty AtomTy }
| "Unit"          { Ty UnitTy }
| "->"            { OFunArr }

| "not"           { ONot }
| "("             { OOpenParen }
| ")"             { OClosedParen }
| "+"             { OPlus }
| "-"             { OMinus }
| "*"             { OStar }
| "/"             { ODivide }
| "="             { OEquals }
| ":"             { OColon }
| ","             { OComma }
| "<>"            { ONotEquals }
| "<"             { OLt }
| ">"             { OGt }
| "<="            { OLte }
| ">="            { OGte }
| "fun"           { KFun }
| "\\"            { KLam }
| ";"             { KSemi }
| "and"           { KAnd }
| "or"            { KOr }

(*
| "nil"           { KNil }
| "type"          { KType }
*)
| ['0'-'9']+ as num
                  { TInt (int_of_string num) }
| ['A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident
                  { Ident ident}
| ['a'-'z' '_']+ as atom
                  { TAtom atom }
(*
| "/*"            { KCommentStart }
| "*/"            { KCommentEnd }
*)
(*
| "&"             { OLogAnd }
| "|"             { OLogOr }
*)
(* 
and

Still needs to be implemented: 
  strings
  *)

