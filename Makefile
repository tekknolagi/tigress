all:
	# menhir --compile-errors l01parser.messages l01parser.mly > l01parser_message.ml
	ocamlbuild -use-ocamlfind -package 'unix' -use-menhir -yaccflags \
		'--infer --explain --strict --unused-tokens' main.native tests.native
	./tests.native
clean:
	ocamlbuild -clean
