ligo_compiler=docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" ligolang/ligo:stable

PROTOCOL_OPT?=

help:
	@echo  'Usage:'
	@echo  '  test            - Run integration tests (written in LIGO)'
	@echo  ''

.PHONY: test

test: test_ligo_trigo_rational test_ligo_trigo_rational

test_ligo_trigo_rational: test/test_trigo_rational.mligo 
	@echo "Running integration tests (trigo rational)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_rational: test/test_rational.mligo 
	@echo "Running integration tests (Rational)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)