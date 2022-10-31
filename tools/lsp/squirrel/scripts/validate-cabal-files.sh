#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Tocqueville Group
#
#i SPDX-License-Identifier: LicenseRef-MIT-TQ

# This script checks that freshly generated cabal files are the same as in the commit
# If cabal files are outdated, this script will push a fixup commit with updated files
set -e

bot_name="CI cabal generator"
# Initializing git variables
our_branch="$CI_MERGE_REQUEST_SOURCE_BRANCH_NAME"

git config --local user.email "hi@serokell.io"
git config --local user.name "$bot_name"
git remote remove auth-origin 2> /dev/null || :
git remote add auth-origin "https://oath2:$GITLAB_PUSH_TOKEN@gitlab.com/serokell/ligo/ligo.git"
git fetch
git checkout -B "$our_branch" --track "origin/$our_branch"

# If last commit was pushed by cabal generator we can leave, the files are already fixed
if [ "$(git show -s --format='%an' HEAD)" == "$bot_name" ]; then
    echo "Skipping duplicated generation step"
    exit 0
fi

# Updating cabal files
stack2cabal

echo "Checking cabal files"
# We add everything here to be able to see new files in the `git diff` below
git add --all

# If no cabal file was changed, exit without errors
if [ -z "$(git diff --name-only --staged)" ]; then
    echo "Cabal files are up-to-date"
    exit 0
fi

# If any cabal file has changed push a fixup commit
echo "Cabal files are outdated, pushing fixup commit"
git commit --fixup HEAD -m "Cabal files update"
git push auth-origin "$our_branch"

# Because we don't want to run other steps in this pipeline.
# Since cabal files are updated in the new commit, all steps will run there
exit 1
