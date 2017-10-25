all:
	ocamlbuild -use-menhir -yaccflags '--infer' main.native
