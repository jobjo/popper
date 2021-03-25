.PHONY: test
all:
	dune build 
test:
	dune runtest --force