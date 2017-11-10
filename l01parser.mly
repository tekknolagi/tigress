%{
  (* header *)
  open Types

  exception ShouldNotHappen of string
  let tyOfFun = function
    | L02ast.Fun(formals, retTy, _, _) ->
        FunTy (List.map snd formals, retTy)
    | _ -> raise @@ ShouldNotHappen "Not given fun"
%}

%token EOL
%token OPlus OMinus OTimes ODivide
%token ONot ONotEquals OLt OGt OLte OGte
%token OEquals OColon OComma
%token OOpenParen OClosedParen
%token KIf KThen KElse KEnd
%token KLet KIn KFun KLam
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
| var
| appexp
| funexp
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

vardecl:
| Ident OColon ty   { ($1, $3) }

var:
| Ident                            { L02ast.Var($1, ()) }
;

appexp:
| var OOpenParen separated_list(OComma, exp) OClosedParen
    { L02ast.App($1, $3, ()) }
| OOpenParen exp OClosedParen OOpenParen separated_list(OComma, exp) OClosedParen
    { L02ast.App($2, $5, ()) }

funexp:
| KFun OOpenParen separated_list(OComma, vardecl) OClosedParen OColon ty
OEquals exp KEnd
| KLam OOpenParen separated_list(OComma, vardecl) OClosedParen OColon ty
OEquals exp KEnd
    { L02ast.Fun($3, $6, $8, ()) }
;

letexp:
| KLet vardecl OEquals exp KIn exp KEnd
      { L02ast.(Let($2, $4, $6, ())) }
| KLet Ident OEquals funexp KIn exp KEnd
      { let ty = tyOfFun $4 in
        L02ast.(Let(($2, ty), $4, $6, ())) }
;

ty:
| Ty    { $1 }
;

%%
