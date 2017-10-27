{
  open L01parser
  exception Eof
}

rule token = parse
| [' ' '\t']        { token lexbuf }
| '\n'            { EOL }
| "true"          { TBool true }
| "false"         { TBool false }
| "if"            { KIf }
| "then"          { KThen }
| "else"          { KElse }
| "end"           { KEnd }
| "let"           { KLet }
| "in"            { KIn }
(*
| "fun"           { KFunction }
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
| "("             { OOpenParen }
| ")"             { OClosedParen }
| "+"             { OPlus }
| "-"             { OMinus }
| "*"             { OTimes }
| "/"             { ODivide }
| "="             { OEquals }
(*
| "<>"            { ONotEquals }
| "<"             { OLt }
| ">"             { OGt }
| "<="            { OLte }
| ">="            { OGte }
| "&"             { OLogAnd }
| "|"             { OLogOr }
*)
(* 
and

Still needs to be implemented: 
  strings
  *)

