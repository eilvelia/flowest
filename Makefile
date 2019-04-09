.PHONY: default
default: build

.PHONY: build
build:
	dune build @install

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

PHONY: distrib
distrib:
	dune-release distrib
