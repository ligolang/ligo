{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "ligo-grammars";

  # use cleanGit from haskell.nix
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "grammar";
    # location relative to git root
    src = ../../../..;
    subDir = "tools/debugger/ligo-debugger/grammar";
  };

  nativeBuildInputs = with pkgs; [ tree-sitter nodejs ];

  HOME = "/tmp";

  installPhase = "cp -r . $out";
}
