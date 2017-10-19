%{
	(* header *)
%}
	(* declaration *)

%token EOL
%start main
%type<unit> main

%%
	(* rules *)
main:
  EOL { () }
;

exp:
| exp (Op Plus) exp 		{ $1 + $2 }
;
%%
