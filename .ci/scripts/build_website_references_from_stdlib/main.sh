#!/usr/bin/env bash 
cd "$(dirname "${BASH_SOURCE[0]}")"

path_to_root=../../..
sidebar_folderpath=$path_to_root/gitlab-pages/website/
# Build the ligo docs mdx
$path_to_root/_build/install/default/bin/ligo doc $path_to_root/src/main/build/ligo_lib/ --mdx
mkdir -p md
mv $path_to_root/src/main/build/ligo_lib/markdown_docs/std_lib.mligo/* md
folder="./md"

echo "Run clean_mdx_generated_header.sh $folder"
./clean_mdx_generated_header.sh $folder

echo "Run add_docusaurus_import.sh $folder"
./add_docusaurus_import.sh $folder

echo "Run fill_mdx_docusaurus_header.sh $folder"
./fill_mdx_docusaurus_header.sh $folder

echo "Run generate_folder_tree.sh $folder"
./generate_folder_tree.sh $folder

echo "Run create_sidebars_references_api_value.sh $folder"
./create_sidebars_references_api_value.sh $folder $sidebar_folderpath

echo "Mergin md/* to $path_to_root/gitlab-pages/docs/reference/"
find md/* -type f -exec cp {} $path_to_root/gitlab-pages/docs/reference/ \;

rm -rf ./md .ligo api_value.js

