build:
	jbuilder build --dev

test:
	jbuilder runtest

all-supported-ocaml-versions:
	jbuilder build --dev @install @runtest --workspace jbuild-workspace.dev --root .

clean:
	jbuilder clean

.PHONY: build all-supported-ocaml-versions clean test
