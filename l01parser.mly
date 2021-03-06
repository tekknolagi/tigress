%{
  (* header *)
  open Types
  open L02ast

  exception ShouldNotHappen of string
  let tyOfFun = function
    | L02ast.Fun(formals, retTy, _, _) ->
        FunTy (List.map snd formals, retTy)
    | _ -> raise @@ ShouldNotHappen "Not given fun"
%}

%token OPlus OMinus OStar ODivide
%token ONot ONotEquals OLt OGt OLte OGte
%token OEquals OColon OComma
%token OOpenParen OClosedParen
%token KIf KThen KElse KEnd
%token KLet KIn KFun KLam KSemi
%token KAnd KOr
%token<int> TInt
%token<bool> TBool
%token<string> TAtom
%token<string> Ident
%token<Types.ty> Ty
%token OFunArr
%left KLet
%left KIf
%left KFun
%left KAnd KOr
%left OLt OGt
%left OLte OGte
%left OPlus OMinus /* lowest precedence */
%left OStar ODivide /* mid-level precedence */
%left ONot
%nonassoc ONotEquals OEquals
%nonassoc OUminus /* highest precedence */
%start main
%type<unit L02ast.exp> main

%%
main:
  exp KSemi { $1 }
;

exp:
| appexp
| funexp %prec KFun
| letexp %prec KLet                { $1 }

| boollit
| intlit
| atomlit
| var
| unitlit                          { $1 }

| OOpenParen exp OClosedParen      { $2 }
| ONot exp                         { L02ast.Not ($2, ()) }
| exp mathop exp                   { L02ast.(Mathop($2, $1, $3, ())) }
| exp cmpop exp                    { L02ast.(Cmpop($2, $1, $3, ())) }
| exp ONotEquals exp               { L02ast.((Not(Cmpop(Equals, $1, $3, ()), ()))) }
| OMinus exp %prec OUminus         { L02ast.(Mathop(Minus, IntLit (0, ()), $2, ())) }
| KIf exp KThen exp KElse exp %prec KIf
                                   { L02ast.IfElse($2, $4, $6, ()) }
| KIf exp KThen exp KEnd           { L02ast.(IfElse($2, $4, UnitLit (), ())) }
;

%inline mathop:
OPlus { Plus } | OMinus { Minus } | OStar { Times } | ODivide { Divide }
;

%inline cmpop:
| OEquals { Equals } | OLt { Lt } | OLte { Lte } | OGt { Gt } | OGte { Gte }
| KAnd { And } | KOr { Or }
;

boollit:
| TBool   { L02ast.BoolLit ($1, ()) }
;

intlit:
| TInt    { L02ast.IntLit ($1, ()) }
;

atomlit:
| TAtom   { L02ast.AtomLit ($1, ()) }
;

unitlit:
| OOpenParen OClosedParen   { L02ast.UnitLit () }
;

vardecl:
| Ident OColon ty   { ($1, $3) }

%inline var:
| Ident                            { L02ast.Var($1, ()) }
;

%inline appexp:
| var OOpenParen separated_list(OComma, exp) OClosedParen
    { L02ast.App($1, $3, ()) }
| OOpenParen exp OClosedParen OOpenParen separated_list(OComma, exp) OClosedParen
    { L02ast.App($2, $5, ()) }

funkw: KFun | KLam { } ;

%inline funargs:
| OOpenParen separated_list(OComma, vardecl) OClosedParen { $2 }
;

%inline funexp:
| funkw funargs OColon ty OEquals exp
    { L02ast.Fun($2, $4, $6, ()) }
;

%inline letexp:
| KLet vardecl OEquals exp KIn exp
      { L02ast.(Let($2, $4, $6, ())) }
| KLet Ident OEquals funexp KIn exp
      { let ty = tyOfFun $4 in
        L02ast.(Let(($2, ty), $4, $6, ())) }
| KLet Ident funargs OColon ty OEquals exp KIn exp
      { let f = L02ast.Fun($3, $5, $7, ()) in
        let ty = tyOfFun f in
        L02ast.(Let(($2, ty), f, $9, ())) }
;

ty:
| Ty    { $1 }
| typaramlist OFunArr Ty { FunTy ($1, $3) }
;

typaramlist:
| Ty    { [$1] }
| OOpenParen separated_list(OStar, ty) OClosedParen { $2 }
;

%%
