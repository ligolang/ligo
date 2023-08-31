#!/bin/sh

something_to_stash_flag=false
if ! git diff --exit-code --quiet; then
  # Stash the unstaged changes
  git stash save --keep-index
  something_to_stash_flag=true
fi

# Run `dune fmt` on the whole project
opam exec -- dune build @fmt --auto-promote

# Check if `dune fmt` made any changes to the files
if git diff --exit-code; then
  echo "Code is properly formatted."
else
  echo "Some files were formatted. Please review the changes."
  echo "You can use 'git diff' to see the changes made by dune fmt."
  echo "If your git index was containing some unstaged files, they are into your stash, run git stash pop to retrieve them"
  exit 1
fi

if $something_to_stash_flag; then
  git stash pop || true
fi
