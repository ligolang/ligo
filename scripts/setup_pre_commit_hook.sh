#!/bin/sh

cp ./scripts/pre_commit_hook.sh .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
echo "pre commit scripts/pre_commit_hook.sh has been installed into .git/hooks."
