.PHONY: default
default: build

.PHONY: build
build:
	dune build @install

.PHONY: clean
clean:
	dune clean

# Full rebuild
.PHONY: rebuild
rebuild: | clean build

.PHONY: test
test:
	dune runtest

.PHONY: distrib
distrib:
	dune-release distrib

.PHONY: lock
lock:
	opam lock

.PHONY: ast-viewer
ast-viewer:
	dune build src/ast_viewer/AstViewer.exe

.PHONY: js
js:
	dune build --profile release src/flowest/Flowest.bc.js \
		&& cp _build/default/src/flowest/Flowest.bc.js packages/flowest/flowest.js
