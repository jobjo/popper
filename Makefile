.PHONY: test format
all:
	dune build 
test:
	dune runtest --force

format:
	dune build @fmt --auto-promote || true

doc:
	dune build @doc