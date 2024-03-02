#!/usr/bin/env bash 
cd "$(dirname "${BASH_SOURCE[0]}")"

files=($(ls $1))

for file in "${files[@]}"
do
  import_libraries="import Syntax from '@theme/Syntax';\\nimport SyntaxTitle from '@theme/SyntaxTitle';\n\n"
  (echo -e "$import_libraries" && cat "./md/$file") > temp && mv temp "./md/"$file""  # transform the file
done
