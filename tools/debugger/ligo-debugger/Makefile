PACKAGE := ligo-debugger

STACK_DEV_OPTIONS = --fast --ghc-options '-Werror -freverse-errors' --file-watch --test --no-run-tests

.DEFAULT_GOAL := all

all: build

FORCE:

clean:
	stack clean

build:
	stack build $(STACK_DEV_OPTIONS)

.PHONY: all clean build FORCE
