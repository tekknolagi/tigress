%{
  (* header *)
  open Types
%}

%token EOL
%token OPlus OMinus OTimes ODivide
%token ONot ONotEquals OLt OGt OLte OGte
%token OEquals OColon
%token OOpenParen OClosedParen
%token KIf KThen KElse KEnd
%token KLet KIn
%token<int> TInt
%token<bool> TBool
%token<string> TAtom
%token<string> Ident
%token<Types.ty> Ty
%left OLt OGt
%left OLte OGte
%left OPlus OMinus /* lowest precedence */
%left OTimes ODivide /* mid-level precedence */
%left ONot
%nonassoc ONotEquals OEquals
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
| ONot exp                         { L02ast.Not ($2, ()) }
| exp OPlus exp                    { L02ast.Plus($1, $3, ()) }
| exp OMinus exp                   { L02ast.Minus($1, $3, ()) }
| exp OTimes exp                   { L02ast.Times($1, $3, ()) }
| exp ODivide exp                  { L02ast.Divide($1, $3, ()) }
| exp OEquals exp                  { L02ast.Equals($1, $3, ()) }
| exp ONotEquals exp               { L02ast.(Not(Equals($1, $3, ()), ())) }
| exp OLt exp                      { L02ast.Lt($1, $3, ()) }
| exp OLte exp                     { L02ast.Lte($1, $3, ()) }
| exp OGt exp                      { L02ast.Gt($1, $3, ()) }
| exp OGte exp                     { L02ast.Gte($1, $3, ()) }
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
