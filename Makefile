# last refactor: 2021-05-05
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
	if [ ! -d "./_opam" ];
	then scripts/setup_switch.sh;
	fi
	eval $$(opam config env)
# NEW-PROTOCOL-TEMPORARY
	git submodule sync --recursive
	git submodule update --init --recursive --remote
# NEW-PROTOCOL-TEMPORARY
#	Install OCaml build dependencies for Ligo
	scripts/install_vendors_deps.sh

build: build-deps
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	scripts/check_duplicate_filenames.sh || exit
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
	scripts/test_ligo.sh

clean:
	dune clean
	rm -fr _coverage_all _coverage_cli _coverage_ligo

coverage:
	eval $$(opam config env)
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html -o ./_coverage_all --title="LIGO overall test coverage"
	bisect-ppx-report summary --per-file

install:
	cp _build/install/default/bin/ligo /usr/local/bin/ligo

install-vscode:
	cd tools/vscode/ && \
	yarn install && \
	rm -f ./*.vsix && \
	yarn package && \
	code --install-extension *.vsix --force

run-vscode: install-vscode
	code

_build/default/src/bin/js_main.bc.js: ./src/bin/js_main.ml ./src/bin/dune
	patch -d vendors/tezos-ligo -p1 < ./0001-Nairobi-JSOO-Gas-free.patch
	patch -d vendors/tezos-ligo -p1 < ./0002-JSOO-Use-lib_hacl-compatible-with-hacl-star-0.4.1.patch
	opam exec -- dune build $(<:.ml=.bc.js)
	patch -d vendors/tezos-ligo -R -p1 < 0001-Nairobi-JSOO-Gas-free.patch
	patch -d vendors/tezos-ligo -R -p1 < 0002-JSOO-Use-lib_hacl-compatible-with-hacl-star-0.4.1.patch


.PHONY: build-demo-webide demo-webide-start
build-demo-webide: _build/default/src/bin/js_main.bc.js
	cd jsoo && env PUPPETEER_PRODUCT=firefox npm i && npm run build

demo-webide-start:
	make build-demo-webide
	python -m http.server -d $(WEB_STAGING_AREA)
