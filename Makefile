all:
	# menhir --compile-errors l01parser.messages l01parser.mly > l01parser_message.ml
	ocamlbuild -use-menhir -yaccflags '--infer --explain --strict --unused-tokens' main.native
clean:
	ocamlbuild -clean
