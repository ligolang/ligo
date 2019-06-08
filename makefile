install:
#	Install opam and dependencies	
	scripts/install_build_environment.sh
#	Install ligo/tezos specific system-level dependencies
	scripts/install_native_dependencies.sh

build-deps:
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	scripts/create_dev_switch.sh
#	Set up the local ligo opam repository so that it can be built
	scripts/setup_ligo_opam_repository.sh
#	Install OCaml build dependencies for Ligo
	scripts/install_ligo_with_dependencies.sh

build: build-deps
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	eval $(opam env)
	dune build @ligo-test
