ligo_compiler?=docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" ligolang/ligo:stable

PROTOCOL_OPT?=

help:
	@echo  'Usage:'
	@echo  '  test            - Run integration tests (written in LIGO)'
	@echo  ''

.PHONY: test

test:
	make -C core $@
	make -C float $@
	make -C rational $@
