install:
	./install.sh

build-deps:
	./build-deps.sh

build: build-deps
	source ./build.sh

test: build
	dune build @ligo-test
