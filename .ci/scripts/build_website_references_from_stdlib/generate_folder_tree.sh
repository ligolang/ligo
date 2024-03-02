#!/usr/bin/env bash 
cd "$(dirname "${BASH_SOURCE[0]}")"
Prefix=$1
files=($(ls $Prefix))

for file in "${files[@]}"; do
  file_without_extension="${file%.*}"
  dirname="${file_without_extension%.*}"
  dirname="${dirname//./\/}"
  if [[ "$dirname" != "$file_without_extension" ]]; then
    dirname=$Prefix/$dirname
    mkdir -p "$dirname"
  else
    dirname=$Prefix
  fi
  mv "$Prefix/$file" "$dirname/$file"
done

# We want to move module to their folder if it exist, so ./Test.md will be at ./Test/Test.md if Test folder exit
find "$Prefix" -type f | while read -r file; do
  file_without_extension="${file%.*}"
  file_basename="${file_without_extension##*/}"
  target=$(basename $file_without_extension)
  target=${target//./\/}
  target=${target///\/\//../}
  mv "$file" "$Prefix/$target/$(basename $file)" || true
done


