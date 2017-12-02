# Tigress

## What is this?

This is my attempt to write a small and easy-to-read compiler.

## How do I build it?

Install `ocaml` (at least 4.02) and `menhir`. Then run `make`.

## What does that produce?

Two binaries: `tests.native`, which is run automatically upon build, and
`main.native`, which contains a REPL. The REPL fluctuates between typechecking,
compiling, and evaluating code. I should probably put some command-line
arguments in there.
