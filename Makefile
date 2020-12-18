all: dev

deps:
	opam install dune ocamlformat odoc angstrom

dev:
	dune build @src/fmt --auto-promote || true
	dune build --profile dev
	dune build @doc
	@echo

repl: dev
	dune exec ./src/repl.exe
	@echo

test: dev
	dune build @runtest -f
	@echo

clean:
	dune clean

.PHONY: all deps dev repl test clean
