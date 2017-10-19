{
  open Parser
  exception Eof
}

rule token = parse
| [' ' '\t'] 				{ token lexbuf }
| '\n' 						{ EOL }
| "true"					{ BoolLit true }
| "false"					{ BoolLit false }
| "if"						{ Keyword If }
| "then"					{ Keyword Then }
| "else" 					{ Keyword Else }
| "end"						{ Keyword End }
| "fun"						{ Keyword Function }
| "in"						{ Keyword In }
| "let"						{ Keyword Let }
| "nil"						{ ListLit Nil }
| "type"					{ Keyword Type }
| '-'? ['0'-'9']+ as num	{ IntLit num }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident 
							{ Ident ident}
| "/*"						{ Keyword CommentStart }
| "*/"						{ Keyword CommentEnd }
| "+"						{ Op Plus }
| "("						{ Op OpenParen }
| ")"						{ Op ClosedParen }
| "-"						{ Op Negation }
| "*"						{ Op Mult }
| "/"						{ Op Divide }
| "="						{ Op Equals }
| "<>"						{ Op NotEquals }
| "<"						{ Op Lt }
| ">"						{ Op Gt }
| "<="						{ Op Lte }
| ">="						{ Op Gte }
| "&"						{ Op LogAnd }
| "|"						{ Op LogOr }

(* Still needs to be implemented: 
	strings
*)

