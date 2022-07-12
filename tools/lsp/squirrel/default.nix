# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ haskell-nix, writeScript, grammars, runCommand, hpack }:
let
  projectSrc = haskell-nix.haskellLib.cleanGit {
    name = "squirrel";
    # location relative to git root
    src = ../../..;
    subDir = "tools/lsp/squirrel";
  };

  clearSymlinks = with haskell-nix.pkgs; contracts: path: (runCommand "cleaned" {} ''
     cp -rL ${contracts}/${path} $out
  '');

  # haskell.nix can generate .cabal file automatically, but it uses a custom
  # build of hpack which requires rebuilding GHC and all haskell dependencies.
  # We use hpack from nixpkgs instead to avoid big rebuilds.
  cabalFile = runCommand "ligo-squirrel.cabal" {} ''
    ${hpack}/bin/hpack ${projectSrc} - > $out
  '';

  project = haskell-nix.stackProject {
    # project src with .cabal file added
    src = runCommand "src-with-cabal" {} ''
      cp -r --no-preserve=mode ${projectSrc} $out
      cp ${cabalFile} $out/ligo-squirrel.cabal
    '';

    ignorePackageYaml = true;

    modules = [
      ({ config, ... }: {
        packages.ligo-squirrel = {
          preBuild = ''
            rm -rf grammar
            cp -r ${grammars} grammar
          '';

          testWrapper = [(toString (writeScript "asdf" ''
            echo 🐿  carry on, it’s just a squirrel

            TEST_DIR=$TMP/contracts
            mkdir -p $TEST_DIR
            cp -rL ${../squirrel} $TEST_DIR/squirrel
            cd $TEST_DIR/squirrel
            chmod -R +w .
            $@
            CODE=$?
            echo 🐿  code $CODE on $@
            echo 🐿  changes in the directory:
            diff -r ${../squirrel} .
            echo 🐿  out
            exit $CODE
          ''))];

          components.tests = {
            ligo-contracts-test = {
              preBuild = ''
                export TEST_DIR=${clearSymlinks ../../.. "src/test"}
                echo $TEST_DIR is our test dir
              '';
            };
          };

          ghcOptions = ["-Werror"];

          # strip library references from the executable to reduce closure size
          dontStrip = false;
        };
      })
    ];
  };
in project.ligo-squirrel
