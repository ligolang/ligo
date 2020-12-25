{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "ligo-grammars";
  src = ./.;

  nativeBuildInputs = with pkgs; [ tree-sitter nodejs ];

  HOME = "/tmp";

  installPhase = "cp -r . $out";
}
