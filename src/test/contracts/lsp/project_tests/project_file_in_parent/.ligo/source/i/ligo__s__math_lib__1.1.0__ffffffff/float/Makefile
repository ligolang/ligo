ligo_compiler?=docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" ligolang/ligo:stable

PROTOCOL_OPT?=

help:
	@echo  'Usage:'
	@echo  '  test            - Run integration tests (written in LIGO)'
	@echo  ''

.PHONY: test

test: test_ligo_float test_ligo_trigo_float

test_ligo_float: test/test_float.mligo 
	@echo "Running integration tests (Float)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_trigo_float: test/test_trigo_float.mligo 
	@echo "Running integration tests (trigo float)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)