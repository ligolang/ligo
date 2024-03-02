#!/usr/bin/env bash 
cd "$(dirname "${BASH_SOURCE[0]}")"

files=($(ls $1))
docu_mdx_header=$(cat docusaurus_mdx_header_template)


for file in "${files[@]}"
do
  title="${file%.md}" # remove .md suffix
  id="${title,,}" # Lowercase
  id="${id//_/-}" # transform _ to -
  title="${title##*.}"
  new_header="${docu_mdx_header//<<id>>/$id}" # Replace <<id>> tag
  new_header="${new_header//<<title>>/$title}"  # Replace <<title>> tag
  (echo -e "$new_header" && cat "./md/$file") > temp && mv temp "./md/"$file""  # transform the file
done
