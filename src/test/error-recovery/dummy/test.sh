#!/usr/bin/env bash

if [[ $# -eq 0 ]] || [[ $# -eq 1 ]] ; then
    echo "Examples of usage:"
    echo "  ./test.sh '*.mligo' cameligo                 # calculate results.csv for all files"
    echo "  ./test.sh test.ligo pascaligo -v             # run calculation on the given file and print diff"
    echo "  ./test.sh 'test1.mligo test2.mligo' cameligo # calculate results.csv only for two files"
    exit 1
fi

DIALECT=$2

# build standalone parser
(cd ../../../../../src/passes/02-parsing/$DIALECT; dune build)

PARSER=./../../../../../_build/default/src/passes/02-parsing/$DIALECT/ParserMain.exe
RESULTS=results.csv

mkdir -p recovered

echo "FILE,LOC,CST,SYMBOLS,TOKENS,ERRORS" > $RESULTS

for f in $1; do
    # f="$(basename -- $f)"
    echo "$f"

    # run parser in different mode to catch errors and the result of error recovery
    $PARSER -- "$f"                     2>&1 > /dev/null \
                 | sed 's/\x1b\[[0-9;]*m//g' > original/"$f.errors" # remove red color
    $PARSER --recovery --pretty -- "$f" 2>&1 > recovered/"$f"\
                 | sed 's/\x1b\[[0-9;]*m//g' > recovered/"$f.errors"
    # NOTE: original/*.errors contains errors that is returned by parser in general mode (without recovery)

    # formate original/ to reduce diff
    $PARSER --pretty -- original/"$f" > original/formatted_"$f"

    # compare string representation of CST
    $PARSER --cst -- original/"$f"                | sed 's/([^()]*)$//' > original/"$f".cst

    $PARSER --recovery --cst -- "$f" 2> /dev/null | sed 's/([^()]*)$//' > recovered/"$f".cst

    # compare list of symbols (nodes of CST)
    cat original/"$f".cst  | sed 's/^\([-|` ]\)*//' > original/"$f".cst_symbols

    cat recovered/"$f".cst | sed 's/^\([-|` ]\)*//' > recovered/"$f".cst_symbols

    # compare list of tokens
    $PARSER --tokens -- original/formatted_"$f"      | sed 's/^[^\ ]*://' > original/"$f".tokens

    $PARSER --tokens --\
            `#workaround: suppress spliting "<invalid-*>" into several tokens`\
            <(sed 's/<invalid-\([^<>]*\)>/_invalid_\1/' recovered/"$f") \
            2> /dev/null  | sed 's/^[^\ ]*://' > recovered/"$f".tokens

    # diff flags
    DF="--suppress-common-lines --side-by-side --minimal"

    # echo "original vs recovery"
    LOC_DIFF=$(    diff $DF --ignore-blank-lines \
                            original/formatted_"$f"    recovered/"$f"             | wc -l | sed 's/ //g')
    CST_DIFF=$(    diff $DF original/"$f".cst          recovered/"$f".cst         | wc -l | sed 's/ //g')
    SYMBOLS_DIFF=$(diff $DF original/"$f".cst_symbols  recovered/"$f".cst_symbols | wc -l | sed 's/ //g')
    TOKENS_DIFF=$( diff $DF original/"$f".tokens       recovered/"$f".tokens      | wc -l | sed 's/ //g')
    ERR_DIFF=$(    diff     original/"$f".errors       recovered/"$f".errors | grep -c "$f")

    # check that old error is preserved
    ERR_DELETED=$(     diff --unchanged-group-format="" --changed-group-format="%<"\
                            original/"$f".errors       recovered/"$f".errors | grep -c "$f")
    if [ $ERR_DELETED != "0" ]; then
        echo "Error is lost!!!"; exit 1
    fi

    echo "$f,$LOC_DIFF,$CST_DIFF,$SYMBOLS_DIFF,$TOKENS_DIFF,$ERR_DIFF" >> $RESULTS

    if [ "$3" = "-v" ]; then
        echo "original vs recovery"
        echo ">>> LOC"
        diff --color original/"formatted_$f" recovered/"$f"
        # echo ">>> CST"
        # diff --color original/"$f".cst recovered/"$f".cst
        # echo ">>> CST symbols"
        # diff --color original/"$f".cst_symbols recovered/"$f".cst_symbols
        # echo ">>> Tokens"
        # diff --color original/"$f".tokens recovered/"$f".tokens

        # echo "original vs test"
        # diff --color original/"$f" "$f"

        echo "test vs recovery"
        diff --color "$f" recovered/"$f"
    fi
done

if [ "$3" = "-v" ]; then
    cat $RESULTS
fi
