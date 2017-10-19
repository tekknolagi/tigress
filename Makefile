all:
	ocamllex l00lexer.mll
	menhir l01parser.mly
	ocamlc -c l01parser.mli
	ocamlc -c l01parser.ml
	ocamlc -c l00lexer.ml
	ocamlc -c main.ml
	ocamlc l01parser.cmo l00lexer.cmo main.cmo
