all:
	ocamllex l00lexer.mll
	menhir l01parser.mly
	ocamlc -c l01parser.mli
	ocamlc -c l01parser.ml
	ocamlc -c l00lexer.ml
	ocamlc -c l02ast.ml
	ocamlc -c main.ml
	ocamlc l00lexer.cmo l01parser.cmo l02ast.cmo main.cmo
