# What

This is tests for error-recovery

# How to run

The main script is [error_recovery.ml](error_recovery.ml).
You can run all tests by `dune build @recovery-test` or from this folder you can run `cd simple/cameligo && dune runtest` or `dune runtest ./simple/cameligo` to evaluate tests for a specific dialect.

# How it works

Every folder contains a list of test cases and `test.ml` file.
Every test case consists of two contracts with the same name.
One of them is incorrect and another one is its correct version in `original` folder.

Test script runs parser without recovery on correct contract to generate its pretty-printed version, cst, cst symbols, and tokens.
It also runs parser without recovery on incorrect contract to collect errors and parser with recovery on incorrect contract to generate pretty-printed version, cst, cst symbols, and tokens of recovered contract and collect errors.

The script checks that the recoverable parser's output preserves an error found by the general parser and contains correct CST.
In case of any unexpected errors the status will be set as FAIL.

After that, the script diffs all corresponding results and counts the number of different lines in `diff`. `0` means good recovery. For some tests, it is impossible to guess the right answer so lower changes are better. [Some tests](#why-some-tests-have-fail-status) have FAIL status in `test.ml` file.

# How to interpret changes between test output and expected in `test.ml`?

If the numbers are not much different, you can run `dune promote` to update `test.ml` with the current result. In the opposite case, your changes may cause regression of error recovery so further investigation is needed.

## `test.ml` expectations description

- STATUS - equals to PASS if all sanity checks passed during testing and equals to FAIL in opposite case.
- LOC — diff between pretty-printed code (amount of lines). It should be very small otherwise, the recovery likely spoiled innocent code around the error.
- CST — diff between printed CST. It catches changes in the structure of the code so e.g. big numbers only here likely mean misinterpreting function scopes.
- CST's symbols — diff between lists of CST's nodes in tree-order (amount of
nodes). It's like the previous but the tree structure is erased so it is focused on content of CST rather than structure.
- Tokens — diff between lists of tokens (amount of tokens). It is focused only on terminal node of CST so it shows how many tokens was inserted or skipped.
- Errors — count of new syntax errors returned by recovery parser. Currently, our tests assume only one mistake so all new errors are caused by a wrong guess of the recovery. So 1-2 is an acceptable value but a bigger one is suspicious.

## How to investigate a problems?

It's hard but possible by use debug tools:
- look at sources of the `results.csv` numbers:
```bash
cd _build/default/src/test/error-recovery/simple/cameligo
diff recovered/problem_test.mligo original_generated/formatted_problem_test.mligo
diff recovered/problem_test.mligo.errors original_generated/problem_test.mligo.errors
diff recovered/problem_test.mligo.cst original_generated/problem_test.mligo.cst
```
- run the parser with the `--trace-recovery` flag to inspect what is happened in the runtime.
```bash
cd src/test/error-recovery/simple/jsligo
(cd ../../../../../src/passes/02-parsing/reasonligo ; dune build)
../../../../../_build/default/src/passes/02-parsing/jsligo/ParserMain.exe --recovery --trace-recovery --pretty -- problem_test.jsligo
```
- to investigate why some recovery candidates is preferred than others look at cost report provided by `menhir-recovery` executable:
```bash
dune exec -- menhir-recover -v _build/default/src/passes/02-parsing/reasonligo/Parser.cmly 2> report.txt > /dev/null
emacs report.txt # or vim report.txt
```
In the end of the `report.txt` you shall find verbose description of actual costs of each recovery candidate, shift and reduce action.
- look at html representation of the parser automaton:
```bash
mkdir graph
dune build src/passes/02-passing/reasonligo
# note '/' in the end of 'graph/'
dune exec -- menhir-recover --graph graph/ _build/default/src/passes/02-parsing/jsligo/Parser.cmly
firefox 0.html # or firefox N.html where N is number of interesting state
```

# Why some tests have FAIL status?

In spite of the error recovery, the parser still can fail if an error turns out before it. For example lexer failure or pre-parser failure
