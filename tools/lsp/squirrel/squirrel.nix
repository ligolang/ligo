# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ linux ? false, linux-static ? false, windows ? false, sources ? import ../nix/sources.nix }:
let
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix" {}).nixpkgsArgs;
  pkgs = if linux-static then nixpkgs.pkgsCross.musl64 else if windows then nixpkgs.pkgsCross.mingwW64 else nixpkgs;
  grammars = (import sources.nixpkgs { }).callPackage ./grammar { };
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.ligo-squirrel = {
        preBuild = ''
          rm vendor/*/*
          ( cd ${grammars}; for i in *; do cp $i/parser.c $NIX_BUILD_TOP/*/vendor/$i; done )
        '';
      };
    }];
  };
in project.ligo-squirrel
