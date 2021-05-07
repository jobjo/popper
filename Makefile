.PHONY: test format examples publish-docs api-docs serve-docs

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

make api-docs:
	dune build @doc

# Serves docs using MkDocs
serve-docs:
	mkdocs serve

# Builds and publishes docs to gh-pages branch
publish-docs: test
	dune build @doc
	mkdocs build
	cp -r _build/default/_doc/_html site/api
	git checkout gh-pages
	cp -r site/* ./
	git commit -am "Latest docs"