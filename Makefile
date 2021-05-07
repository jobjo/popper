.PHONY: test format examples
all:
	dune build 

clean:
	dune clean

test:
	dune runtest --force

examples:
	dune exec examples/run.exe

format:
	dune build @fmt --auto-promote || true

doc: test
	dune build @doc
	mkdocs build
	cp -r _build/default/_doc/_html site/api
	git checkout gh-pages
	cp -r site/* ./
	git commit -am "Latest docs"


