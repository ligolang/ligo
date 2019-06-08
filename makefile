# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh

build-deps:
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	scripts/setup_dev_switch.sh
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
