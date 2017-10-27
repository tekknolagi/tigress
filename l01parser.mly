%{
  (* header *)
  open Types
%}

%token EOL
%token OPlus OMinus OTimes ODivide
%token OEquals OColon
%token OOpenParen OClosedParen
%token KIf KThen KElse KEnd
%token KLet KIn
%token<int> TInt
%token<bool> TBool
%token<string> TAtom
%token<string> Ident
%token<Types.ty> Ty
%left OPlus OMinus /* lowest precedence */
%left OTimes ODivide /* mid-level precedence */
%nonassoc OUminus /* highest precedence */
%start main
%type<unit L02ast.exp> main

%%
main:
  exp EOL { $1 }
;

exp:
| boollit
| intlit
| unitlit                          { $1 }
| OOpenParen exp OClosedParen      { $2 }
| exp OPlus exp                    { L02ast.Plus($1, $3, ()) }
| exp OMinus exp                   { L02ast.Minus($1, $3, ()) }
| exp OTimes exp                   { L02ast.Times($1, $3, ()) }
| exp ODivide exp                  { L02ast.Divide($1, $3, ()) }
| OMinus exp %prec OUminus         { L02ast.(Minus(IntLit (0, ()), $2, ())) }
| KIf exp KThen exp KElse exp KEnd { L02ast.IfElse($2, $4, $6, ()) }
| KIf exp KThen exp KEnd           { L02ast.(IfElse($2, $4, UnitLit (), ())) }
| TAtom                            { L02ast.AtomLit($1, ()) }
| Ident                            { L02ast.Var($1, ()) }
| letexp                           { $1 }
;

boollit:
| TBool   { L02ast.BoolLit ($1, ()) }
;

intlit:
| TInt    { L02ast.IntLit ($1, ()) }
;

unitlit:
| OOpenParen OClosedParen   { L02ast.UnitLit () }
;

letexp:
| KLet Ident OColon ty OEquals exp KIn exp KEnd
      { L02ast.(Let($2, $4, $6, $8, ())) }
;

ty:
| Ty    { $1 }

%%
