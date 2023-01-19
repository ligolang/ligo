#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

DEB_PKG_URL=$1
CURRENT_VERSION=$2

TEMPLATE_PKGBUILD_PATH="./template/template.PKGBUILD"
NEW_PKGBUILD_PLACE="./ligo.PKGBUILD"

# Place the template to the good place
cp $TEMPLATE_PKGBUILD_PATH $NEW_PKGBUILD_PLACE

# Fill the template which has been placed
./fill_template.sh $NEW_PKGBUILD_PLACE $DEB_PKG_URL $CURRENT_VERSION
