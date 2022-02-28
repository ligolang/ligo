#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")/../../gitlab-pages/website"
 
CURRENT_VERSION=$1

# retrieve version to delete from versions.json (last of the array)
VERSION_TO_DELETE=`jq '.[-1]' versions.json | tr -d '"'`

# Archive version
npm ci docusaurus
npm run docusaurus docs:version $CURRENT_VERSION


############################################
# REMOVE OLDEST VERSION
# We want to keep only 3 versions
############################################

# versioned_docs/version-$VERSION_TO_DELETE
rm -rf versioned_docs/version-$VERSION_TO_DELETE
# versioned_sidebars/version-$VERSION_TO_DELETE-sidebars.json
rm versioned_sidebars/version-$VERSION_TO_DELETE-sidebars.json
# versions.json (delete last entry which represent the oldest version)
jq "del(.[3])"  versions.json > versions.tmp && mv versions.tmp versions.json
