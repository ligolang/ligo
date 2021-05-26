# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ haskell-nix, grammars }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "squirrel";
      # location relative to git root
      src = ../../..;
      subDir = "tools/lsp/squirrel";
    };

    modules = [
      ({ config, ... }: {
        packages.ligo-squirrel = {
          preBuild = ''
            rm -rf grammar
            cp -r ${grammars} grammar
          '';

          # Thanks, I Hate It.
          components.tests.ligo-contracts-test = {
            preBuild = "export CONTRACTS_DIR=${../../../src/test/contracts}";
          };

          package.ghcOptions = "-Werror";

          # strip library references from the executable to reduce closure size
          dontStrip = false;
        };
      })
    ];
  };
in project.ligo-squirrel
