# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ platform ? "linux-static" }:

(import ./squirrel.nix { ${platform} = true; }).components.exes.squirrel
