# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ haskell-nix, grammars, tree }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.ligo-squirrel = {
        preBuild = ''
          rm -rf grammar
          cp -r ${grammars} grammar
        '';
      };
    }];
  };
in project.ligo-squirrel
