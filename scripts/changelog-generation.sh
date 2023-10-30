#!/usr/bin/env nix-shell
#!nix-shell -p mustache-go jq  -i bash

# shellcheck shell=bash

cd "$(dirname "${BASH_SOURCE[0]}"/..)"
cache_dir="$HOME/.ligo_changelog_cache"
cache_hash=$(find ./changelog -type f -exec md5sum {} + | sort -k 2 | md5sum | awk '{print $1}')
activate_cache=true

generate_changelog() {
  ./scripts/changelog-json.sh > changelog.json
  jq '
    .changelog[].changes |=
    with_entries (
      if .key | in({"breaking":1,"added":1,"fixed":1,"changed":1,"deprecated":1,"removed":1,"performance":1,"internal":1,"other":1,"null":1})
      then
        .value[].description |=
        if . != null
        then
          gsub("\n";"\n  ")
        else
          .
        end
      else
        .
      end
    )' \
  < changelog.json \
  > changelog_indented.json

  mustache ./changelog_indented.json ./scripts/changelog.md.mustache > changelog.md
  mustache ./changelog.json ./scripts/changelog.txt.mustache > changelog.txt

  jq '.changelog[0]' changelog.json > release-notes.json

  mustache ./release-notes.json ./scripts/release-notes.md.mustache > release-notes.md
  mustache ./release-notes.json ./scripts/release-notes.txt.mustache > release-notes.txt

  #save in cache
  if [ "$activate_cache" = "true" ]; then
    save_in_cache $cache_hash
  fi
}

delete_old_cache() { 
  local number_of_day=14
  find "$cache_dir" -type f -mtime +${number_of_day} -exec rm {} \;
}

copy_from_cache() {
  local changelog_cache="$cache_dir/$cache_hash"
  local changelog_md_cache="$changelog_cache/changelog.md"
  local changelog_txt_cache="$changelog_cache/changelog.txt"
  local release_notes_md_cache="$changelog_cache/release-notes.md"
  local release_notes_txt_cache="$changelog_cache/release-notes.txt"

  if [ -f "$changelog_md_cache" ] && [ -f "$changelog_txt_cache" ] && [ -f "$release_notes_md_cache" ] && [ -f "$release_notes_txt_cache" ]; then
    cp "$changelog_md_cache" changelog.md
    cp "$changelog_txt_cache" changelog.txt
    cp "$release_notes_md_cache" release-notes.md
    cp "$release_notes_txt_cache" release-notes.txt
    echo "Changelog copied from cache."
  else
    echo "Changelog not found in cache. Generating changelog..."
    [ -f "$changelog_cache" ] && rm "$changelog_cache"
    [ -f "$changelog_txt_cache" ] && rm "$changelog_txt_cache"
    [ -f "$release_notes_md_cache" ] && rm "$release_notes_md_cache"
    [ -f "$release_notes_txt_cache" ] && rm "$release_notes_txt_cache"
    generate_changelog
  fi
}

save_in_cache() {
  local changelog_cache="$cache_dir/$cache_hash"
  mkdir -p $changelog_cache
  cp changelog.md $changelog_cache/changelog.md
  cp changelog.txt $changelog_cache/changelog.txt
  cp release-notes.md $changelog_cache/release-notes.md
  cp release-notes.txt $changelog_cache/release-notes.txt
  echo "Changelog saved to cache."
}

if [ "$activate_cache" = "true" ]; then
  copy_from_cache $cache_hash
  delete_old_cache
else
  echo "cache filenames not provided. Generating changelog..."
  generate_changelog
fi
