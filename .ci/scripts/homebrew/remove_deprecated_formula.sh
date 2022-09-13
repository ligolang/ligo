#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

vercomp () {
    if [[ $1 == $2 ]]
    then
        return 0
    fi
    local IFS=.
    local i ver1=($1) ver2=($2)
    # fill empty fields in ver1 with zeros
    for ((i=${#ver1[@]}; i<${#ver2[@]}; i++))
    do
        ver1[i]=0
    done
    for ((i=0; i<${#ver1[@]}; i++))
    do
        if [[ -z ${ver2[i]} ]]
        then
            # fill empty fields in ver2 with zeros
            ver2[i]=0
        fi
        if ((10#${ver1[i]} > 10#${ver2[i]}))
        then
            return 1
        fi
        if ((10#${ver1[i]} < 10#${ver2[i]}))
        then
            return 2
        fi
    done
    return 0
}


HOMEBREW_FORMULA_FOLDER_PATH=$1
CURRENT_VERSION=$2

NUMBER_OF_VERSION_TO_KEEP=3

VERSION_REGEX="([0-9]*)\.([0-9]*)\.([0-9]*)"
EXTRACT_VERSION_REGEX="$HOMEBREW_FORMULA_FOLDER_PATH/([0-9]*\.[0-9]*\.[0-9]*).rb$"
FILES=($(find $HOMEBREW_FORMULA_FOLDER_PATH -type f -regex "$HOMEBREW_FORMULA_FOLDER_PATH/[0-9]*\.[0-9]*\.[0-9]*\.rb$"))

if [[ $CURRENT_VERSION =~ $VERSION_REGEX ]]; then
  NEW_MAJOR=${BASH_REMATCH[1]}
  NEW_MINOR=${BASH_REMATCH[2]} 
  NEW_FIX=${BASH_REMATCH[3]}
  echo "Current version:  $NEW_MAJOR $NEW_MINOR $NEW_FIX "
else
  echo "VERSION provided is not semserv."
fi

OLDEST_VERSION="9999.9999.9999"
# check if anterior fix version exist
for FILEPATH in "${FILES[@]}"
do

  if [[ $FILEPATH =~ $EXTRACT_VERSION_REGEX ]]; then
    FILE_VERSION=${BASH_REMATCH[1]}
  else
    echo "no match found, seems like a bug in code."
  fi

  #VERSION_TO_REMOVE
  vercomp $OLDEST_VERSION $FILE_VERSION
  if [[ $? = 1 ]]; then
      OLDEST_VERSION=$FILE_VERSION
  fi
  echo $OLDEST_VERSION
  if [[ $FILE_VERSION =~ $VERSION_REGEX ]]; then
    MAJOR=${BASH_REMATCH[1]}
    MINOR=${BASH_REMATCH[2]} 
    FIX=${BASH_REMATCH[3]}
  else
    echo "no match found, seems like a bug in code."
  fi

  echo "if [[ $MAJOR = $NEW_MAJOR ]] && [[ $MINOR = $NEW_MINOR ]] && [[ $FIX -lt $NEW_FIX ]] "
  if [[ $MAJOR = $NEW_MAJOR ]] && [[ $MINOR = $NEW_MINOR ]] && [[ $FIX -lt $NEW_FIX ]]; then 
    echo "fix version detected, remove the old one"
    rm $HOMEBREW_FORMULA_FOLDER_PATH/$MAJOR.$MINOR.$FIX.rb
    exit 0
  fi
done

# If there is no files, exit directly.
if [[ ${#FILES[@]} -le $NUMBER_OF_VERSION_TO_KEEP ]]; then
 echo "there is less than 4 version, keep them all"
 exit 0
fi

rm $HOMEBREW_FORMULA_FOLDER_PATH/$OLDEST_VERSION.rb

