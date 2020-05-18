#!/bin/sh

apt-get update -qq
apt-get -y -qq install jq

fetch_version () {
  local LAST_VERSION=`curl --silent "https://gitlab.com/api/v4/projects/12294987/repository/tags?search=^V&order_by=name" | jq "map(.name)[0]"`
  MAJOR=`echo $LAST_VERSION | sed -E "s/\.|\"|\V/\n/g" | grep -e . | sed -n 1p`
  MINOR=`echo $LAST_VERSION | sed -E "s/\.|\"|\V/\n/g" | grep -e . | sed -n 2p`
  PATCH=`echo $LAST_VERSION | sed -E "s/\.|\"|\V/\n/g" | grep -e . | sed -n 3p`
}
increment_patch () {
  fetch_version
  local NEW_PATCH=$((PATCH+1))
  NEW_VERSION="${MAJOR}.${MINOR}.${NEW_PATCH}"
}
increment_minor () {
  fetch_version
  local NEW_MINOR=$((MINOR+1))
  NEW_VERSION="${MAJOR}.${NEW_MINOR}.0"
}
increment_major () {
  fetch_version
  local NEW_MAJOR=$((MAJOR+1))
  NEW_VERSION="${NEW_MAJOR}.0.0"
}

tag_dev () {
  curl --request POST --header "PRIVATE-TOKEN: ${AUTH}" https://gitlab.com/api/v4/projects/12294987/repository/tags -d "tag_name=V.${1}&ref=dev"
}


increment_minor
echo $NEW_VERSION
# increment_major
# echo $NEW_VERSION
# increment_patch
# echo $NEW_VERSION

tag_dev $NEW_VERSION


# curl --header "PRIVATE-TOKEN: W-7UVDzeofRmejE17_Gn" https://gitlab.com/api/v4/version
# curl --request POST --header "PRIVATE-TOKEN: <your_access_token>" https://gitlab.example.com/api/v4/projects/5/repository/branches?branch=newbranch&ref=master

