{
  stdenv,
  lib,
  fetchFromGitHub,
  tree-sitter,
}: let
  version = "0.22.5";

  src = fetchFromGitHub {
    owner = "tree-sitter";
    repo = "tree-sitter-typescript";
    rev = "198d03553f43a45b92ac5d0ee167db3fec6a6fd6";
    hash = "sha256-U597+o8gakd4nU9H2FE2aVhGqSG/eRh6BUhtEmwMzrU=";
  };
in
  stdenv.mkDerivation {
    pname = "tree-sitter-typescript";
    inherit src version;

    nativeBuildInputs = [tree-sitter];

    configurePhase = ''
      cd typescript
    '';

    makeFlags = ["PREFIX=${placeholder "out"}"];
  }
