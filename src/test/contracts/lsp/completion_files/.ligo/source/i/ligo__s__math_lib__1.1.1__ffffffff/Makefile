ligo_compiler?=docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" ligolang/ligo:1.0.0

PROTOCOL_OPT?=

help:
	@echo  'Usage:'
	@echo  '  test            - Run integration tests (written in LIGO)'
	@echo  ''

.PHONY: test

test: test_ligo_utils test_ligo_math test_ligo_float test_ligo_trigo_float  test_ligo_trigo_rational test_ligo_trigo_rational

test_ligo_trigo_rational: rational/test/test_trigo_rational.mligo 
	@echo "Running integration tests (trigo rational)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_rational: rational/test/test_rational.mligo 
	@echo "Running integration tests (Rational)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_float: float/test/test_float.mligo 
	@echo "Running integration tests (Float)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_trigo_float: float/test/test_trigo_float.mligo 
	@echo "Running integration tests (trigo float)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_utils: core/test/test_utils.mligo 
	@echo "Running integration tests (is_implicit, bytes_to_nat)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_math: core/test/test_math.mligo 
	@echo "Running integration tests (Math)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)
