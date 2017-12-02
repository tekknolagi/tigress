# Tigress

## What is this?

This is my attempt to write a small and easy-to-read compiler.

## How do I build it?

Install `ocaml` (at least 4.02) and `menhir`. Then run `make`.

## What does that produce?

Two binaries: `tests.native`, which is run automatically upon build, and
`main.native`, which contains a REPL.

The REPL has several different modes which are described in the `mode` type in
[main.ml](main.ml). You can select one by doing, for example,

```
oak% rlwrap ./main.native parse
REPL in parse mode.
>>> let Fact(X:Int):Int = if X < 2 then 1 else X*Fact(X-1) in Fact(5);
Let (Fact, Fun ([X : Int], IfElse (Lt (Var X, IntLit 2), IntLit 1, Times (Var X, App (Var Fact, [Minus (Var X, IntLit 1)])))), App (Var Fact, [IntLit 5]))
>>>
```

or

```
oak% rlwrap ./main.native eval
REPL in eval mode.
>>> let Fact(X:Int):Int = if X < 2 then 1 else X*Fact(X-1) in Fact(5);
120
>>>
```
