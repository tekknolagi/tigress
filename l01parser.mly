%{
  (* header *)
%}
  (* declaration *)

%token EOL
%token OPlus
%token<int> IntLit
%left OPlus /* lowest precedence */
%start main
%type<int> main

%%
  (* rules *)
main:
  exp EOL { $1 }
;

exp:
| exp OPlus exp     { $1 + $3 }
| intlit            { $1 }
;

intlit:
| IntLit { $1 }
;
%%
