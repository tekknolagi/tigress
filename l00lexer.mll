{
  open L01parser
  exception Eof
}

rule token = parse
| [' ' '\t']        { token lexbuf }
| '\n'            { EOL }
| "true"          { BoolLit true }
| "false"         { BoolLit false }
(*
| "if"            { KIf }
| "then"          { KThen }
| "else"          { KElse }
| "end"           { KEnd }
| "fun"           { KFunction }
| "in"            { KIn }
| "let"           { KLet }
| "nil"           { KNil }
| "type"          { KType }
*)
| ['0'-'9']+ as num  { IntLit (int_of_string num) }
(*
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident 
                  { Ident ident}
| "/*"            { KCommentStart }
| "*/"            { KCommentEnd }
*)
| "("             { OOpenParen }
| ")"             { OClosedParen }
| "+"             { OPlus }
| "-"             { OMinus }
| "*"             { OTimes }
| "/"             { ODivide }
(*
| "="             { OEquals }
| "<>"            { ONotEquals }
| "<"             { OLt }
| ">"             { OGt }
| "<="            { OLte }
| ">="            { OGte }
| "&"             { OLogAnd }
| "|"             { OLogOr }
*)

(* Still needs to be implemented: 
  strings
  *)

