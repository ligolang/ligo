SITTER ?= tree-sitter

DIRS := grammar/pascaligo grammar/reasonligo grammar/camligo

PACKAGE := squirrel

STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch

.DEFAULT_GOAL := all

all: build

FORCE:

generate: $(DIRS)

$(DIRS): FORCE
	# $(SITTER) generate # Generation is manual for now
	mkdir -p $(PACKAGE)/vendor/$@
	cp $@/src/parser.c vendor/$@/parser.c
	cp -r $@/src/tree_sitter vendor/$@/tree_sitter

clean:
	$(RM) vendor/*
	stack clean

build:
	stack build $(STACK_DEV_OPTIONS)

.PHONY: all clean generate build FORCE
