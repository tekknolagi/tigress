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
| "+"						
| "("						
| ")"						
| "-"						
| "*"						
| "/"						
| "="						
| "<>"						
| "<"						
| ">"						
| "<="						
| ">="						
| "&"						
| "|" as op

{ Operator op }
(* Still needs to be implemented: 
	strings
*)
