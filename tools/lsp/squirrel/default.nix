# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ platform ? "linux-static" }:

(import ./ligo-squirrel.nix { ${platform} = true; }).components.exes.ligo-squirrel
