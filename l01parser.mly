%{
  (* header *)
%}
  (* declaration *)

%token EOL
%token OPlus OMinus OTimes ODivide
%token OOpenParen OClosedParen
%token<int> IntLit
%left OPlus OMinus /* lowest precedence */
%left OTimes ODivide /* mid-level precedence */
%nonassoc OUminus
%start main
%type<int> main

%%
  (* rules *)
main:
  exp EOL { $1 }
;

exp:
| intlit             { $1 }
| OOpenParen exp OClosedParen { $2 }
| exp OPlus exp      { $1 + $3 }
| exp OMinus exp     { $1 - $3 }
| exp OTimes exp     { $1 * $3 }
| exp ODivide exp     { $1 / $3 }
| OMinus exp %prec OUminus         { - $2 }
;

intlit:
| IntLit { $1 }
;
%%
