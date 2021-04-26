.PHONY: test format examples
all:
	dune build 
test:
	dune runtest --force

examples:
	dune exec examples/run.exe

format:
	dune build @fmt --auto-promote || true

doc:
	dune build @doc