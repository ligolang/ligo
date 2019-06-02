install:
	./install.sh

build-deps:
	./build-deps.sh

build: build-deps
	./build.sh

test: build
	eval $(opam env)
	dune build @ligo-test
