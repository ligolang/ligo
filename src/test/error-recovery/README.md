# What

This is tests for error-recovery

# How to run

The main script is [test.sh](test.sh). 
You can run all tests by `dune runtest` from this folder or you can run `cd simple/cameligo && dune runtest` or `dune runtest ./simple/cameligo` to evaluate tests for a specific dialect.

# How it works

Every folder contains a list of test cases and `results.csv.expected` file.
Every test case consists of two contracts with the same name. 
One of them is incorrect and another one is its correct version in `original` folder.

Test script runs parser without recovery on correct contract to generate its pretty-printed version, cst, cst symbols, and tokens.
It also runs parser without recovery on incorrect contract to collect errors and parser with recovery on incorrect contract to generate pretty-printed version, cst, cst symbols, and tokens of recovered contract and collect errors.

The script will fail if any of those runs will be finished with non-zero code.
The script also fails if there is an error in the parser without recovery that is not presented in errors of the parser with recovery.
In case of failing the name of the test will be printed in red instead of green.

After that, the script diffs all corresponding results and writes the number of different lines in `diff` to `results.csv`. `0` means good recovery. For some tests, it is impossible to guess the right answer so lower changes are better.
The last check is that generated `results.csv` is equal to `results.csv.expected`.
**In case if they are not but the numbers are not much different, you can run `dune promote` to copy your `results.csv` to `results.csv.expected`. In the opposite case, your changes greatly affect recovery.**.

# Results.csv description

- LOC -- diff between pretty-printed code (amount of lines)
- CST -- diff between printed CST (amount of nodes)
- CST's symbols -- diff between lists of CST's nodes in tree-order (amount of
nodes). Like the previous but the tree structure is erased.
- Tokens -- diff between lists of tokens
- Errors -- count of new syntax error returned by recovery parser