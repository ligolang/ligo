#! /bin/bash

ROOT="${PWD}"
PROJECTS="${ROOT}/src/test/projects"
CONTRACTS="${ROOT}/src/test/contracts"

function test_output() {
    TEST_NAME="$1"
    if test -n "$2"; then
	echo "$2" | awk '{$1=$1;print}' | cmp -n 10 - "${ROOT}/e2e/${TEST_NAME}.out"
    elif test ! -t 0; then
	cat | awk '{$1=$1;print}' | cmp -n 10 - "${ROOT}/e2e/${TEST_NAME}.out"
    else
        echo "No standard input."
    fi
}

ligo --version

TEST_NAME="using_scope_pkg_project"
cd "${PROJECTS}/${TEST_NAME}"
ligo install
ligo run test ./src/a/b/c/contract.test.mligo | test_output "${TEST_NAME}"

TEST_NAME="aggregation"
cd "${CONTRACTS}/${TEST_NAME}"
ligo compile contract "effects.mligo" | test_output "${TEST_NAME}"
