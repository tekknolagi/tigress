%token EOL
%start main
%type<unit> main
%%
main:
  EOL { () }
;
