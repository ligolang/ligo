PACKAGE := squirrel

LIGO_SEVERITY = LIGO_SEVERITY="debug"
LIGO_ENV = LIGO_ENV="development"
BUILD_FLAGS = $(LIGO_SEVERITY) $(LIGO_ENV)
GHC_OPTIONS_WITH_WERROR = --fast --test --no-run-tests --ghc-options '-Werror -freverse-errors'
GHC_OPTIONS_WITHOUT_WERROR = --fast --test --no-run-tests --ghc-options '-freverse-errors'

STACK_DEV = $(BUILD_FLAGS) stack build

.DEFAULT_GOAL := all

all: watch

FORCE:

grammar: ; $(MAKE) -C $@

clean:
	$(RM) -r grammar/*/src
	$(RM) grammar/*/binding.gyp
	stack clean

build: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITHOUT_WERROR)

werror: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITH_WERROR)

werror-watch: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITH_WERROR) --file-watch

watch: grammar
	$(STACK_DEV) $(GHC_OPTIONS_WITHOUT_WERROR) --file-watch

.PHONY: all clean grammar build watch FORCE
