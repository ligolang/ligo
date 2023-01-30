PACKAGE := squirrel

LIGO_SEVERITY = LIGO_SEVERITY="debug"
LIGO_ENV = LIGO_ENV="development"
LIGO_LSP_TIMESTATS = LIGO_LSP_TIMESTATS="1"
VSCODE_BIN_PATH = ../vscode-plugin/bin
BUILD_FLAGS = $(LIGO_SEVERITY) $(LIGO_ENV)
GHC_OPTIONS_WITH_WERROR    = --fast --test --no-run-tests --bench --no-run-benchmarks --ghc-options '-Werror -freverse-errors'
GHC_OPTIONS_WITHOUT_WERROR = --fast --test --no-run-tests --bench --no-run-benchmarks --ghc-options '-freverse-errors'
GHC_OPTIONS_PROFILE        =        --test --no-run-tests --bench --no-run-benchmarks --ghc-options '-freverse-errors -fprof-auto -with-rtsopts=-p'

STACK_DEV = $(BUILD_FLAGS) stack build

.DEFAULT_GOAL := all

all: watch

FORCE:

grammar: ; $(MAKE) -C $@

build: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITHOUT_WERROR)

werror: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITH_WERROR)

werror-watch: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITH_WERROR) --file-watch

watch: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITHOUT_WERROR) --file-watch

installvsc: grammar
	$(BUILD_FLAGS) stack install :ligo-squirrel $(GHC_OPTIONS_WITHOUT_WERROR) --local-bin-path $(VSCODE_BIN_PATH)
	cd ../vscode-plugin && yarn package && code --install-extension *.vsix

installvsc-profile: grammar
	$(LIGO_LSP_TIMESTATS) $(BUILD_FLAGS) stack install :ligo-squirrel $(GHC_OPTIONS_PROFILE) --local-bin-path $(VSCODE_BIN_PATH) --profile
	cd ../vscode-plugin && yarn package && code --install-extension *.vsix
	# Restart the VSCode to get an LSP with profiling, and then open a LIGO file to launch it.
	# Profiling file "ligo-squirrel.prof" will appear in ~/.vscode/extensions/ligolang-publish.ligo-vscode-*.*.*/ after LSP shutdown.
	# This file can be explored via ghc-prof-flamegraph.
	# Also there is a ligo-squirrel-timestats file with CPU time and calls counters for different functions.

stylish:
	find . -name 'submodules' -prune -o -name '.stack-work' -prune -o -name '.dist-newstyle' -prune -o -name '*.hs' -exec stylish-haskell -i '{}' \;

# examples: make bench filter="Complex", make bench filter=" \\\"Complex benchmarks, Standard/one_big_file references keystrokes\\\""
# You can bench other executable, e.g. LIGO_LSP_TEST_EXE="ligo-lsp-exe" make bench
bench: grammar
	git submodule update --init --recursive
	$(BUILD_FLAGS) stack bench --ba "-v2 $(filter)"
# Cleaning up. FIXME: LSP should do cleanup on shutdown, see https://issues.serokell.io/issue/LIGO-691
	rm -f bench/projects/*/.ligo-work/*; rm -f bench/submodules/baseDAO/src/.ligo-work/*;
.PHONY: all FORCE grammar build werror werror-watch watch installvsc installvsc-profile stylish bench
