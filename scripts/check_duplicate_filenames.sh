#!/bin/bash

ALL=`find src | awk '{print tolower($0)}' | sort`

UNIQUE=`echo "$ALL" | uniq`

diff --color=always <( echo "$ALL" ) <( echo "$UNIQUE" )

if [[ $? != 0 ]]
then
    echo "There are files with duplicate names, check above diff"
    exit 1
fi