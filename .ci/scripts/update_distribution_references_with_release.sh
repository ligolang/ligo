#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

LAST_TAG_JOB_ID=$1
CURRENT_VERSION=$2

ROOT_FOLDER="../.."

DISTRIBUTION_URL_PATTERN_DEB_NEXT="https://ligolang.org/deb/ligo.deb"
DISTRIBUTION_URL_PATTERN_BINARY_NEXT="https://ligolang.org/bin/linux/ligo"

DISTRIBUTION_URL_GITLAB_ARTIFACT_REGEX_PATTERN_RELEASE="(.*https://gitlab\.com/ligolang/ligo/-/jobs/)[0-9]{10}(/artifacts/raw\/(ligo\.deb|ligo))"
VERSION_REGEX_PATTERN="[0-9]+\.[0-9]+.[0.9]+"

DEB_GITLAB_ARTIFACT_URL="https://gitlab.com/ligolang/ligo/-/jobs/$1/artifacts/raw/ligo.deb"
BINARY_GITLAB_ARTIFACT_URL="https://gitlab.com/ligolang/ligo/-/jobs/$1/artifacts/raw/ligo"

FILES_PATH_TO_EDIT=(
    "$ROOT_FOLDER/tools/webide/Dockerfile"
    "$ROOT_FOLDER/gitlab-pages/doc/intro/installation.md
)

for VERSION_FOLDER in `ls $ROOT_FOLDER/gitlab-pages/website/versioned_docs/`
do
    FILES_PATH_TO_EDIT+=("$ROOT_FOLDER/gitlab-pages/website/versioned_docs/$VERSION_FOLDER/intro/installation.md")
done
 
SED_IN_PLACE_COMMAND=(sed -i)
if [ "$(uname)" == "Darwin" ]; then
    # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x 
    SED_IN_PLACE_COMMAND=(sed -i '')
fi

for filepath in "${FILES_PATH_TO_EDIT[@]}"
do
    # Update if versionned gitlab artifact is already used 
    "${SED_IN_PLACE_COMMAND[@]}" -E "s#$DISTRIBUTION_URL_GITLAB_ARTIFACT_REGEX_PATTERN_RELEASE#\1$LAST_TAG_JOB_ID\2#" $filepath
    # If next version is provided, update it with gitlab versioned artifact
    "${SED_IN_PLACE_COMMAND[@]}" "s#$DISTRIBUTION_URL_PATTERN_DEB_NEXT#$DEB_GITLAB_ARTIFACT_URL#" $filepath
    "${SED_IN_PLACE_COMMAND[@]}" "s#$DISTRIBUTION_URL_PATTERN_BINARY_NEXT#$BINARY_GITLAB_ARTIFACT_URL#" $filepath

    "${SED_IN_PLACE_COMMAND[@]}" -E "s|$VERSION_REGEX_PATTERN|$CURRENT_VERSION|g" $filepath
done
