.ONESHELL:

all: test

# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh
	scripts/install_build_environment.sh # TODO: or scripts/install_opam.sh ?

build-deps:
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	if [ -n "`opam switch show | grep -P ".+/ligo"`" ];
	then :; else scripts/setup_switch.sh;
	fi
	scripts/setup_repos.sh
	eval $$(opam config env)
#	Install OCaml build dependencies for Ligo
	scripts/install_vendors_deps.sh

build: build-deps
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
	scripts/test_ligo.sh

clean:
	dune clean

coverage: clean
	BISECT_ENABLE=yes dune runtest --force
	bisect-ppx-report html -o ./_coverage_all --title="LIGO overall test coverage"
	bisect-ppx-report summary --per-file

coverage-ligo: clean
	BISECT_ENABLE=yes dune runtest src/test --force
	bisect-ppx-report html -o ./_coverage_ligo --title="LIGO test coverage"
	bisect-ppx-report summary --per-file

coverage-cli: clean
	BISECT_ENABLE=yes dune runtest src/bin/expect_tests
	bisect-ppx-report html -o ./_coverage_cli --title="CLI test coverage"
	bisect-ppx-report summary --per-file
	# PRE="CLI coverage: "
	# bisect-ppx-report summary --per-file | grep 'src/bin/cli.ml\|src/bin/cli_helpers.ml' | awk -v ORS=" " '{print $1}' | awk -v PRE=${PRE} -v POST="%" '{print PRE ($1 + $2 / NF) POST}'