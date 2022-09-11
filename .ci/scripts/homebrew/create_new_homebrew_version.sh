#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"
ROOT_PATH="../../.."

PROJECT_URL=$1
PROJECT_ID=$2
LAST_TAG_JOB_ID=$3
CURRENT_VERSION=$4
GITLAB_REF_PAYLOAD=$(./retrieve_gitlab_reference_info.sh $PROJECT_URL $PROJECT_ID $CURRENT_VERSION)
GIT_SHA_REFERENCE=$(jq .id <<< ${GITLAB_REF_PAYLOAD} | tr -d "\"")

TEMPLATE_FORMULA_PATH="./template/homebrew-template.rb"
HOMEBREW_FORMULA_FOLDER_PATH="$ROOT_PATH/HomebrewFormula"
NEW_FORMULA_PLACE="$HOMEBREW_FORMULA_FOLDER_PATH/$CURRENT_VERSION.rb"
LATEST_FORMULA_PLACE="$ROOT_PATH/HomebrewFormula/ligo.rb"

# Place the template to the good place
cp $TEMPLATE_FORMULA_PATH $NEW_FORMULA_PLACE

# Fill the template which has been placed
./fill_template.sh $NEW_FORMULA_PLACE $LAST_TAG_JOB_ID $CURRENT_VERSION $GIT_SHA_REFERENCE

cp $NEW_FORMULA_PLACE $LATEST_FORMULA_PLACE

./remove_deprecated_formula.sh $HOMEBREW_FORMULA_FOLDER_PATH $CURRENT_VERSION 
