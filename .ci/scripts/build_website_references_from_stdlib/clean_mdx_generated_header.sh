#!/usr/bin/env bash 
cd "$(dirname "${BASH_SOURCE[0]}")"

files=($(find "$1" -mindepth 1 -maxdepth 1 -type f))
for file in "${files[@]}"
do
  # Remove the generated name
  echo -e "$(tail -n +2 "$file")">$file
done
