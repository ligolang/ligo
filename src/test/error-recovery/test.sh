#!/usr/bin/env bash

diff_lines () {
    stat=$(git diff --numstat --minimal --no-index -- "$1" "$2")
    if [ -z "$stat" ]; then
        echo 0
    else
        read insertion deletion rest <<<$stat
        echo $(($insertion + $deletion))
    fi
}

print_usage () {
    echo "Examples of usage:"
    echo "  ./test.sh '*.mligo' ParserMain.exe                   # calculate results.csv for all files"
    echo "  ./test.sh test.jsligo ../../jsligo/ParserMain.exe -v # run calculation on the given file and print diff"
    echo "  ./test.sh 'test1.mligo test2.mligo' ParserMain.exe   # calculate results.csv only for two files"
    echo "  ./test.sh test.jsligo ../../jsligo/ParserMain.exe -v # run calculation on the given file and print diff"
    echo "  ./test.sh 'test1.mligo test2.mligo' ParserMain.exe   # calculate results.csv only for two files"
    echo "  ./test.sh cameligo <dune-workspace-root> simple      # calculate results.csv for simple error-recovery tests"
    echo "  ./test.sh cameligo <dune-workspace-root> fuzzing     # calculate results.csv for fuzzing error-recovery tests"
}

LC_ALL=C

if [[ $# -eq 0 ]] || [[ $# -eq 1 ]] ; then
    print_usage;
    exit 1
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# process command line options
while test $# -ge 1
do
case "$1" in
    -h* | --help)
	print_usage;
        exit 0 ;;
    --test-type=*)
	TEST_TYPE=`echo $1 | sed 's/.*=//'`;
	if [ -z "$TEST_TYPE" ]
	then
	    echo "No TEST_TYPE passed. Valid options are: fuzzing or simple"
	    exit 1;
	fi
	case "$TEST_TYPE" in
	    fuzzing)
		;;
	    simple)
		;;
	    *)
		echo "Unrecognised test type '$TEST_TYPE'. Valid options are: fuzzing or simple"
		exit 1;
		;;
	esac
	shift;
	;;
    --dune-workspace=*)
	DUNE_WORKSPACE_ROOT=`echo $1 | sed 's/.*=//'`;
	if [ -z "$DUNE_WORKSPACE_ROOT" ];
	then
	    echo "Dune workspace root was not passed"
	    exit 1;
	fi

	if [ ! -d "$DUNE_WORKSPACE_ROOT" ];
	then
	    echo "Dune workspace root, $DUNE_WORKSPACE_ROOT, doesn't exist. Please pass a valid one"
	    exit 1;
	fi
	shift;
	;;
    --dialect=*)
	DIALECT=`echo $1 | sed 's/.*=//'`;
	if [ -z "$DIALECT" ]
	then
	    echo "No dialect passed. Valid dialects are: cameligo or jsligo"
	    exit 1;
	fi
	case "$DIALECT" in
	    cameligo)
		LIGO_FILE_EXT="mligo";
		;;
	    jsligo)
		LIGO_FILE_EXT="jsligo";
		;;
	    *)
		echo "Unrecognised ligo dialect '$DIALECT'. Valid dialects are: cameligo or jsligo"
		exit 1;
		;;
	esac
	shift;
	;;
    *)
	LIGO_FILES=$1
	PARSER=$2
	break;
	;;
    esac
done

if [ -z "$LIGO_FILES" ]
then

    case "$(uname -s)" in
	Darwin*)
	    LIGO_FILES=$(find $DUNE_WORKSPACE_ROOT/src/test/error-recovery/$TEST_TYPE/$DIALECT -name "*.$LIGO_FILE_EXT" -type f -exec basename {} + | sort | uniq)
	    ;;
	*)
	    LIGO_FILES=$(find $DUNE_WORKSPACE_ROOT/src/test/error-recovery/$TEST_TYPE/$DIALECT -name "*.$LIGO_FILE_EXT" -type f -printf "%f\n" | sort | uniq)
	    ;;
    esac
fi

if [ -z "$PARSER" ]
then
    PARSER="$DUNE_WORKSPACE_ROOT/src/passes/02-parsing/$DIALECT/ParserMain.exe"
fi

RESULTS=results.csv

mkdir -p recovered
mkdir -p original_generated

echo "STATUS,FILE,LOC,CST,SYMBOLS,TOKENS,ERRORS" > $RESULTS

for f in $LIGO_FILES; do
    CURRENT_FILE_ERROR=0
    # enable trap to catch all nonzero exit code
    trap 'CURRENT_FILE_ERROR=1' ERR

    # run parser in different mode to catch errors and the result of error recovery
    $PARSER -- "$f"                     2>&1 > /dev/null \
                 | sed -e $'s/\x1b\[[0-9;]*m//g' > original_generated/"$f.errors" # remove red color
    $PARSER --recovery --pretty -- "$f" 2>&1 > recovered/"$f"\
                 | sed -e $'s/\x1b\[[0-9;]*m//g' > recovered/"$f.errors"
    # NOTE: original_generated/*.errors contains errors that is returned by parser in general
    #       mode (without recovery)

    # run parser on the recovery output and check that it is not empty
    if [[ $(cat recovered/"$f") ]]; then
        $PARSER -- recovered/"$f"
    else
        if [ "$3" = "-v" ]; then
            echo -e $RED"ERROR:$f recovery output is empty."$NC
            cat recovered/"$f.errors"
        fi
        CURRENT_FILE_ERROR=1
    fi

    # formate original/ to reduce diff
    $PARSER --pretty -- original/"$f" > original_generated/formatted_"$f"

    # compare string representation of CST
    $PARSER --cst -- original/"$f"                | sed 's/([^()]*)$//' > original_generated/"$f".cst
    $PARSER --recovery --cst -- "$f" 2> /dev/null | sed 's/([^()]*)$//' > recovered/"$f".cst

    # compare list of symbols (nodes of CST)
    cat original_generated/"$f".cst  | sed 's/^\([-|` ]\)*//' > original_generated/"$f".cst_symbols
    cat recovered/"$f".cst | sed 's/^\([-|` ]\)*//' > recovered/"$f".cst_symbols

    # compare list of tokens
    $PARSER --tokens -- original_generated/formatted_"$f" | sed 's/^[^\ ]*://' > original_generated/"$f".tokens
    $PARSER --tokens --\
            `#workaround: suppress spliting "<invalid-*>" into several tokens`\
            <(sed 's/<invalid-\([^<>]*\)>/_invalid_\1/' recovered/"$f") \
            2> /dev/null  | sed 's/^[^\ ]*://' > recovered/"$f".tokens

    # disable trap because diff utilities return nonzero code if files aren't the same
    trap - ERR

    LOC_DIFF=$(    diff_lines original_generated/formatted_"$f"   recovered/"$f")
    CST_DIFF=$(    diff_lines original_generated/"$f".cst         recovered/"$f".cst)
    SYMBOLS_DIFF=$(diff_lines original_generated/"$f".cst_symbols recovered/"$f".cst_symbols)
    TOKENS_DIFF=$( diff_lines original_generated/"$f".tokens      recovered/"$f".tokens)
    ERR_DIFF=$(git diff --no-index -U0 -- original_generated/"$f".errors recovered/"$f".errors \
                | tail -n +6 | grep -c "$f")

    # check that old error is preserved
    ERR_DELETED=$(git diff --no-index -- original_generated/"$f".errors recovered/"$f".errors \
            | grep '^-[^-]' | grep -c "$f")
    if [ $ERR_DELETED != "0" ]; then
        echo -e $RED"ERROR:$f: the recovery lost the original error!!!"$NC
        CURRENT_FILE_ERROR=1
    fi


    if [ $CURRENT_FILE_ERROR != "0" ]; then
        echo "FAIL,$f,-1,-1,-1,-1,-1" >> $RESULTS
    else
        echo "PASS,$f,$LOC_DIFF,$CST_DIFF,$SYMBOLS_DIFF,$TOKENS_DIFF,$ERR_DIFF" >> $RESULTS
    fi

    if [ "$3" = "-v" ]; then
        echo "original vs recovery"
        echo ">>> LOC"
        diff --color original_generated/"formatted_$f" recovered/"$f"
        # echo ">>> CST"
        # diff --color original_generated/"$f".cst recovered/"$f".cst
        # echo ">>> CST symbols"
        # diff --color original_generated/"$f".cst_symbols recovered/"$f".cst_symbols
        # echo ">>> Tokens"
        # diff --color original_generated/"$f".tokens recovered/"$f".tokens

        # echo "original vs test"
        # diff --color original_generated/"$f" "$f"

        echo "test vs recovery"
        diff --color "$f" recovered/"$f"
    fi
done

if [ "$3" = "-v" ]; then
    cat $RESULTS
fi

pass=$(grep -c PASS results.csv)
fail=$(grep -c FAIL results.csv)
echo "PASS: $pass, FAIL: $fail"

exit 0
