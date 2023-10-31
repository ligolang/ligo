#! /usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# This script relinks macOS binary to use non-system libraries from the adjacent directory,
# thus this binary can be reused on any macOS machine with the same versions of system libraries.
# It accepts full path to the binary as a first argument, and relative path
# (relative to the first argument) to the directory where the libraries will be stored
#
# Example: ./relink-mac-binary.sh $PWD/bin/ligo ../lib
# Binary will be linked to the libraries in '$PWD/lib'

set -euo pipefail

executable_to_relink="$1"
relative_library_path="$2"
mkdir -p "$(dirname "$executable_to_relink")/$relative_library_path"

lib_system="libSystem.B.dylib"

relink_file() {
    local file_to_relink
    local file_name
    file_to_relink="$1"
    file_name="$(basename "$file_to_relink" | xargs)"
    echo "Changing library links for: $file_to_relink"
    local otool_output
    otool_output="$(otool -L "$file_to_relink" | tail -n +2)"
    otool_output=${otool_output//	/}
    IFS=$'\n'
    for lib in $otool_output; do
        local lib_path
        lib_path="$(echo "$lib" | cut -d '(' -f 1 | xargs)"
        local lib_name
        lib_name="$(basename "$lib_path")"
        if [[ "$lib_name" != "$file_name" && "$lib_path" =~ $(brew --prefix) ]]; then
            if [[ "$lib_name" != "$lib_system" ]]; then
                local lib_local_path
                lib_local_path="$(dirname "$executable_to_relink")/$relative_library_path/$lib_name"
                echo "Changing $lib_path to $lib_local_path"
                cp -n "$lib_path" "$(dirname "$executable_to_relink")/$relative_library_path/"
                chmod +w "$lib_local_path"
                install_name_tool -change "$lib_path" "@executable_path/$relative_library_path/$lib_name" \
                                "$file_to_relink"
                relink_file "$(dirname "$executable_to_relink")/$relative_library_path/$lib_name"
            fi
        fi
    done
}

relink_file "$executable_to_relink"
