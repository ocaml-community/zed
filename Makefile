build:
	dune build

test:
	dune runtest

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev

clean:
	dune clean

.PHONY: build all-supported-ocaml-versions clean test
